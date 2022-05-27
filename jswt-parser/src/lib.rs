mod class;
mod errors;
mod function;

pub use errors::ParseError;
use std::vec;

use jswt_ast::*;
use jswt_common::{Span, Spannable};
use jswt_tokenizer::{Token, TokenType, Tokenizer, TokenizerError};

type ParseResult<T> = Result<T, ParseError>;

/// Returns true if a token matching the given token type
/// was consumed.
#[macro_export]
macro_rules! maybe_consume {
    ($self:ident, $token:expr) => {{
        if $self.lookahead_is($token) {
            Some(consume_unchecked!($self))
        } else {
            None
        }
    }};
}

/// Advances the tokenizer to the next token. This assumes
/// that the caller has done their due dilligence of checking that
/// the current lookahead token is what was expected
#[macro_export]
macro_rules! consume_unchecked {
    ($self:ident) => {{
        let span = $self.lookahead_span();
        $self.lookahead = $self.tokenizer.next_token();
        span
    }};
}

/// Checks that the current lookahead tokentype is the same type
/// as the given token then advances the tokenizer to the next token
#[macro_export]
macro_rules! consume {
    ($self:ident, $token:expr) => {{
        // TODO - this should generate a parse error, not panic
        let token = $self.lookahead.as_ref().expect("Unexpected end of input");
        if token.kind != $token {
            return Err(ParseError::MismatchedToken {
                expected: $token,
                actual: token.kind,
                span: $self.lookahead_span(),
            });
        }
        // Advance lookahead
        let span = $self.lookahead_span();
        $self.lookahead = $self.tokenizer.next_token();
        Ok::<Span, ParseError>(span)
    }};
}

#[macro_export]
macro_rules! ident {
    // This macro takes an argument of designator `ident` and
    // creates a function named `$func_name`.
    // The `ident` designator is used for variable/function names.
    ($self:ident) => {{
        let token = $self.lookahead.as_ref().expect("unexpected end of input");
        if token.kind != TokenType::Identifier {
            return Err(ParseError::MismatchedToken {
                expected: TokenType::Identifier,
                actual: token.kind,
                span: $self.lookahead_span(),
            });
        }
        let ident = Identifier::new(token.span.lexme(), token.span.clone());
        // Advance lookahead
        $self.lookahead = $self.tokenizer.next_token();
        Ok::<Identifier, ParseError>(ident)
    }};
}

macro_rules! binary_expression {
    ($name:ident, next: $next:ident, $([exp => $exp:ident, op => $op:ident, token => $token:ident]),*) => {
        fn $name(&mut self) -> ParseResult<SingleExpression>{
            let mut left = self.$next()?;
            while let Some(token) = self.lookahead_type() {
                match token {
                    $(
                        TokenType::$token => {
                            let op_span = consume_unchecked!(self);
                            let right = self.$next()?;
                            left = SingleExpression::$exp(BinaryExpression {
                                span: left.span() + right.span(),
                                left: Box::new(left),
                                op: BinaryOperator::$op(op_span),
                                right: Box::new(right),
                            });
                        }
                    )*
                    _ => break,
                }
            }

            Ok(left)
        }
    };
}

/// Predictive LL(1) parser
pub struct Parser<'a> {
    tokenizer: &'a mut Tokenizer,
    lookahead: Option<Token>,
    errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    pub fn new(tokenizer: &'a mut Tokenizer) -> Parser {
        Self {
            tokenizer,
            lookahead: None,
            errors: vec![],
        }
    }

    pub fn parse(&mut self) -> Ast {
        // Seed the look ahead for the entry point
        self.lookahead = self.tokenizer.next_token();
        Ast::new(self.program())
    }

    pub fn parse_statement(&mut self) -> StatementElement {
        // Seed the look ahead for the entry point
        self.lookahead = self.tokenizer.next_token();
        self.statement().unwrap()
    }

    /// Entry point of the program
    ///
    /// Program
    ///   :  File*
    ///   ;
    fn program(&mut self) -> Program {
        let mut files = vec![];
        while self.lookahead.is_some() {
            files.push(self.file());
        }
        Program { files }
    }

    /// File
    ///   : SourceElements? Eof
    ///   ;
    fn file(&mut self) -> File {
        let start = self.lookahead_span();
        let source_elements = self.source_elements(Some(TokenType::Eof));
        // Eat the EOF token
        let end = consume_unchecked!(self);
        File {
            span: start + end,
            source_elements,
        }
    }

    /// SourceElements
    ///   :  SourceElement
    ///   |  SourceElements SourceElement
    ///   ;
    fn source_elements(&mut self, terminal: Option<TokenType>) -> SourceElements {
        let mut source_elements = vec![];
        let start = self.lookahead_span();
        while self.lookahead_type().is_some() && self.lookahead_type() != terminal {
            match self.source_element() {
                Ok(element) => source_elements.push(element),
                Err(err) => self.handle_error_and_recover(
                    err,
                    &[
                        // FunctionDeclaration start tokens
                        TokenType::Export,
                        TokenType::Function,
                        // Statement start tokens
                        TokenType::LeftBrace,
                        TokenType::Semi,
                        TokenType::Return,
                        TokenType::Let,
                        TokenType::Const,
                    ],
                ),
            };
        }

        let end = self.lookahead_span();
        SourceElements {
            span: start + end,
            source_elements,
        }
    }

    /// SourceElement
    ///   :  FunctionDeclaration
    ///   |  ClassDeclaration
    ///   |  Statement
    ///   ;
    fn source_element(&mut self) -> ParseResult<SourceElement> {
        let elem = match self.lookahead_type() {
            // Need to check for optional function decorators
            Some(TokenType::Function) | Some(TokenType::Export) | Some(TokenType::At) => {
                self.function_declaration()?.into()
            }
            Some(TokenType::Class) => self.class_declaration()?.into(),
            _ => self.statement()?.into(),
        };
        Ok(elem)
    }

    /// Statement
    ///   :  Block
    ///   |  EmptyStatement
    ///   |  IfStatement
    ///   |  IterationStatement
    ///   |  ReturnStatement
    ///   |  VariableStatement
    ///   |  ExpressionStatement
    ///   ;
    fn statement(&mut self) -> ParseResult<StatementElement> {
        let elem = match self.lookahead_type() {
            Some(TokenType::LeftBrace) => self.block()?.into(),
            Some(TokenType::Semi) => self.empty_statement()?.into(),
            Some(TokenType::If) => self.if_statement()?.into(),
            Some(TokenType::While) => self.iteration_statement()?.into(),
            Some(TokenType::Return) => self.return_statement()?.into(),
            Some(TokenType::Let) | Some(TokenType::Const) => self.variable_statement()?.into(),
            _ => self.expression_statement()?.into(),
        };
        Ok(elem)
    }

    /// Block
    ///   :  '{' statementList? '}'
    ///   ;
    fn block(&mut self) -> ParseResult<BlockStatement> {
        let start = consume!(self, TokenType::LeftBrace)?;
        let statements = self.statement_list(Some(TokenType::RightBrace))?;
        let end = consume!(self, TokenType::RightBrace)?;
        Ok(BlockStatement {
            span: start + end,
            statements,
        })
    }

    /// EmptyStatement
    ///   : ';'
    ///   ;
    fn empty_statement(&mut self) -> ParseResult<EmptyStatement> {
        let span = consume!(self, TokenType::Semi)?;
        Ok(EmptyStatement { span })
    }

    /// IfStatement
    ///   : 'if' '(' SingleExpression ')' BlockStatement 'else' BlockStatement
    ///   | 'if' '(' SingleExpression ')' BlockStatement
    ///   ;
    fn if_statement(&mut self) -> ParseResult<IfStatement> {
        let start = consume!(self, TokenType::If)?;
        consume!(self, TokenType::LeftParen)?;
        let condition = self.single_expression()?;
        consume!(self, TokenType::RightParen)?;
        let consequence = Box::new(self.statement()?);

        let mut alternative = None;
        if self.lookahead_is(TokenType::Else) {
            consume!(self, TokenType::Else)?;
            alternative = Some(Box::new(self.statement()?));
        }

        let end = alternative
            .as_ref()
            .map(|alt| alt.span())
            .unwrap_or_else(|| consequence.span());

        Ok(IfStatement {
            span: start + end,
            condition,
            consequence,
            alternative,
        })
    }

    /// IterationStatement
    ///   :  WhileStatement
    ///   ;
    fn iteration_statement(&mut self) -> ParseResult<IterationStatement> {
        let elem = match self.lookahead_type() {
            Some(TokenType::While) => self.while_statement()?,
            _ => todo!(),
        };

        Ok(elem)
    }

    /// WhileStatement
    ///   : 'while' '(' SingleExpression ')' Statement
    ///   ;
    fn while_statement(&mut self) -> ParseResult<IterationStatement> {
        let start = consume!(self, TokenType::While)?;
        consume!(self, TokenType::LeftParen)?;
        let expression = self.single_expression()?;
        consume!(self, TokenType::RightParen)?;
        let block = self.block()?;

        Ok(WhileIterationElement {
            span: start + block.span(),
            expression,
            block,
        }
        .into())
    }

    /// ReturnStatement
    ///   : 'return' SingleExpression ';'
    ///   ;
    fn return_statement(&mut self) -> ParseResult<ReturnStatement> {
        let start = consume!(self, TokenType::Return)?;
        let expression = self.single_expression()?;
        let end = consume!(self, TokenType::Semi)?;

        Ok(ReturnStatement {
            span: start + end,
            expression,
        })
    }

    /// StatementList
    ///   :  Statement
    ///   |  StatementList Statement
    ///   ;
    fn statement_list(&mut self, terminal: Option<TokenType>) -> ParseResult<StatementList> {
        let mut statements = vec![];
        while self.lookahead_type().is_some() && self.lookahead_type() != terminal {
            match self.statement() {
                Ok(element) => statements.push(element),
                Err(err) => self.handle_error_and_recover(
                    err,
                    &[
                        TokenType::LeftBrace,
                        TokenType::RightBrace,
                        TokenType::Semi,
                        TokenType::Return,
                        TokenType::Let,
                        TokenType::Const,
                    ],
                ),
            }
        }
        Ok(StatementList { statements })
    }

    /// VariableStatement
    ///   :  VariableModifier Assignable ('=' singleExpression)? ';'
    ///   ;
    fn variable_statement(&mut self) -> ParseResult<VariableStatement> {
        let modifier = self.variable_modifier()?;
        let target = self.assignable()?;

        let mut type_annotation = None;
        if self.lookahead_is(TokenType::Colon) {
            type_annotation = Some(self.type_annotation()?);
        }

        consume!(self, TokenType::Equal)?;
        let expression = self.single_expression()?;
        let end = consume!(self, TokenType::Semi)?;

        Ok(VariableStatement {
            span: modifier.span() + end,
            modifier,
            target,
            expression,
            type_annotation,
        })
    }

    /// VariableModifier
    ///   : 'let'
    ///   | 'const'
    ///   ;
    fn variable_modifier(&mut self) -> ParseResult<VariableModifier> {
        let modifier = match self.lookahead_type().unwrap() {
            TokenType::Let => VariableModifier::Let,
            TokenType::Const => VariableModifier::Const,
            // it should never be anything but these
            _ => {
                let lookahead = self.lookahead.as_ref().unwrap();
                return Err(ParseError::NoViableAlternative {
                    expected: vec![TokenType::Let, TokenType::Const],
                    actual: lookahead.kind,
                    span: lookahead.span.clone(),
                });
            }
        };
        // Eat the modifier token
        let span = consume_unchecked!(self);
        Ok(modifier(span))
    }

    /// Assignable
    ///   : Ident
    ///   ;
    fn assignable(&mut self) -> ParseResult<AssignableElement> {
        Ok(AssignableElement::Identifier(ident!(self)?))
    }

    /// ExpressionStatement
    ///   : SingleExpression ';'
    ///   ;
    fn expression_statement(&mut self) -> ParseResult<ExpressionStatement> {
        let expression = self.single_expression()?;
        let end = consume!(self, TokenType::Semi)?;

        Ok(ExpressionStatement {
            span: expression.span() + end,
            expression,
        })
    }

    /// SingleExpression
    ///   : SingleExpression '=' SingleExpression
    ///   | SingleExpression '.' Identifier
    ///   | 'new' SingleExpression
    ///   | SingleExpression '[' SingleExpression ']'
    ///   | SingleExpression '|' SingleExpression
    ///   | SingleExpression '&' SingleExpression
    ///   | SingleExpression ('==' | '!=') SingleExpression
    ///   | SingleExpression ('<' | '>' | '<=' | '>=') SingleExpression
    ///   | SingleExpression ('+' | '-') SingleExpression
    ///   | SingleExpression ('*' | '/') SingleExpression
    ///   | '!' SingleExpression
    ///   | '-' SingleExpression
    ///   | '+' SingleExpression
    ///   | SingleExpression Arguments
    ///   | 'this'
    ///   | Identifier
    ///   | ArrayLiteral
    ///   | Literal
    ///   ;
    pub fn single_expression(&mut self) -> ParseResult<SingleExpression> {
        self.assignment_expression()
    }

    // AssignmentExpression
    //   : MemberDotExpression '=' MemberDotExpression
    //   ;
    binary_expression!(
        assignment_expression,
        next: member_dot_expression,
        [exp => Assignment, op => Assign, token => Equal]
    );

    /// MemberDotExpression
    ///   : NewExpression '.' Identifier ArgumentsList?
    ///   ;
    fn member_dot_expression(&mut self) -> ParseResult<SingleExpression> {
        // target.expression
        let target = self.new_expression()?;
        if self.lookahead_is(TokenType::Dot) {
            consume_unchecked!(self);
            let expression = self.new_expression()?;
            return Ok(SingleExpression::MemberDot(MemberDotExpression {
                span: target.span() + expression.span(),
                expression: Box::new(expression),
                target: Box::new(target),
            }));
        }
        Ok(target)
    }

    /// NewExpression
    ///   : 'new' SingleExpression
    ///   ;
    fn new_expression(&mut self) -> ParseResult<SingleExpression> {
        if self.lookahead_is(TokenType::New) {
            let start = consume_unchecked!(self);
            let expression = self.single_expression()?;
            return Ok(SingleExpression::New(NewExpression {
                span: start + expression.span(),
                expression: Box::new(expression),
            }));
        }

        self.member_index_expression()
    }

    /// MemberIndexExpression
    ///   : BitwiseOrExpression '[' BitwiseOrExpression ']'
    ///   ;
    fn member_index_expression(&mut self) -> ParseResult<SingleExpression> {
        let target = self.bitwise_or_expression()?;
        if self.lookahead_is(TokenType::LeftBracket) {
            consume_unchecked!(self);

            let index = self.bitwise_or_expression()?;
            let end = consume!(self, TokenType::RightBracket)?;
            return Ok(SingleExpression::MemberIndex(MemberIndexExpression {
                span: target.span() + end,
                target: Box::new(target),
                index: Box::new(index),
            }));
        }
        Ok(target)
    }

    //  BitwiseOrExpression
    //    : BitwiseAndExpression '|' BitwiseAndExpression
    //    ;
    binary_expression!(
        bitwise_or_expression,
        next: bitwise_and_expression,
        [exp => Bitwise, op => Or, token => Or]
    );

    //  BitwiseAndExpression
    //    : EqualityExpression '&' EqualityExpression
    //    ;
    binary_expression!(
        bitwise_and_expression,
        next: equality_expression,
        [exp => Bitwise, op => And, token => And]
    );

    // EqualityExpression
    //    : RelationalExpression ('==' | '!=') RelationalExpression
    //    ;
    binary_expression!(
        equality_expression,
        next: relational_expression,
        [exp => Equality, op => Equal, token => EqualEqual],
        [exp => Equality, op => NotEqual, token => BangEqual]
    );

    //  RelationalExpression
    //    : AdditiveExpression ('<' | '>' | '<=' | '>=') AdditiveExpression
    //    ;
    binary_expression!(
        relational_expression,
        next: additive_expression,
        [exp => Relational, op => Greater, token => Greater],
        [exp => Relational, op => GreaterEqual, token => GreaterEqual],
        [exp => Relational, op => Less, token => Less],
        [exp => Relational, op => LessEqual, token => LessEqual]
    );

    // AdditiveExpression
    //    : MultiplicativeExpression ('+' | '-') MultiplicativeExpression
    //    ;
    binary_expression!(
        additive_expression,
        next: multipicative_expression,
        [exp => Additive, op => Plus, token => Plus],
        [exp => Additive, op => Minus, token => Minus]
    );

    // MultiplicativeExpression
    //    : PostfixUnaryExpression ('*' | '/' | '%') PostfixUnaryExpression
    //    ;
    binary_expression!(
        multipicative_expression,
        next: postfix_unary_expression,
        [exp => Multiplicative, op => Mult, token => Star],
        [exp => Multiplicative, op => Div, token => Slash]
    );

    // PostfixUnaryExpression
    //    : PrefixUnaryExpression ('++' | '--')
    //    ;
    fn postfix_unary_expression(&mut self) -> ParseResult<SingleExpression> {
        let expr = self.prefix_unary_expression()?;
        if self.lookahead_is(TokenType::PlusPlus) {
            let op = consume_unchecked!(self);
            return Ok(SingleExpression::Unary(UnaryExpression {
                span: op.to_owned() + expr.span(),
                op: UnaryOperator::PostIncrement(op),
                expr: Box::new(expr),
            }));
        }
        if self.lookahead_is(TokenType::MinusMinus) {
            let op = consume_unchecked!(self);
            return Ok(SingleExpression::Unary(UnaryExpression {
                span: op.to_owned() + expr.span(),
                op: UnaryOperator::PostDecrement(op),
                expr: Box::new(expr),
            }));
        }
        Ok(expr)
    }

    /// PrefixUnaryExpression
    ///   : '+' ArgumentsExpression
    ///   | '-' ArgumentsExpression
    ///   | '!' ArgumentsExpression
    ///   ;
    fn prefix_unary_expression(&mut self) -> ParseResult<SingleExpression> {
        if self.lookahead_is(TokenType::Plus) {
            let op = consume_unchecked!(self);
            let expr = self.arguments_expression()?;
            return Ok(SingleExpression::Unary(UnaryExpression {
                span: op.to_owned() + expr.span(),
                op: UnaryOperator::Plus(op),
                expr: Box::new(expr),
            }));
        }

        if self.lookahead_is(TokenType::Minus) {
            let op = consume_unchecked!(self);
            let expr = self.arguments_expression()?;
            return Ok(SingleExpression::Unary(UnaryExpression {
                span: op.to_owned() + expr.span(),
                op: UnaryOperator::Minus(op),
                expr: Box::new(expr),
            }));
        }

        if self.lookahead_is(TokenType::Not) {
            let op = consume_unchecked!(self);
            let expr = self.arguments_expression()?;
            return Ok(SingleExpression::Unary(UnaryExpression {
                span: op.to_owned() + expr.span(),
                op: UnaryOperator::Not(op),
                expr: Box::new(expr),
            }));
        }

        self.arguments_expression()
    }

    /// ArgumentsExpression
    ///   :  IdentifierExpression ArgumentList
    ///   ;
    ///
    fn arguments_expression(&mut self) -> ParseResult<SingleExpression> {
        // Eventually descend to ident
        let left = self.this()?;
        if self.lookahead_is(TokenType::LeftParen) {
            let args = self.argument_list()?;
            return Ok(SingleExpression::Arguments(ArgumentsExpression {
                span: left.span() + args.span(),
                ident: Box::new(left),
                arguments: args,
            }));
        }

        Ok(left)
    }

    /// ArgumentList
    ///   :  '(' SingleExpression (',' SingleExpression)* ')'
    ///   ;
    fn argument_list(&mut self) -> ParseResult<ArgumentsList> {
        let mut arguments = vec![];
        let start = consume!(self, TokenType::LeftParen)?;
        while !self.lookahead_is(TokenType::RightParen) {
            let param = self.single_expression()?;
            arguments.push(param);
            if !self.lookahead_is(TokenType::Comma) {
                break;
            }
            // Eat the comma
            consume_unchecked!(self);
        }
        let end = consume!(self, TokenType::RightParen)?;
        // return params
        Ok(ArgumentsList {
            span: start + end,
            arguments,
        })
    }

    /// ThisExpression
    ///   :  'this'
    ///   ;
    fn this(&mut self) -> ParseResult<SingleExpression> {
        if self.lookahead_is(TokenType::This) {
            let span = consume!(self, TokenType::This)?;
            return Ok(SingleExpression::This(ThisExpression { span }));
        }
        self.identifier_expression()
    }

    /// IdentifierExpression
    ///   : Identifier
    ///   ;
    fn identifier_expression(&mut self) -> ParseResult<SingleExpression> {
        if self.lookahead_is(TokenType::Identifier) {
            let ident = ident!(self)?;
            return Ok(SingleExpression::Identifier(IdentifierExpression {
                span: ident.span.to_owned(),
                ident,
            }));
        }
        // If we can't find an ident, continue by
        // trying to resolve an array literal
        self.array_literal_expression()
    }

    /// ArrayLiteral
    ///   :  '[' (SingleExpression ,)* ']'
    ///   ;
    fn array_literal_expression(&mut self) -> ParseResult<SingleExpression> {
        if self.lookahead_is(TokenType::LeftBracket) {
            let start = consume_unchecked!(self);

            let mut elements = vec![];
            while !self.lookahead_is(TokenType::RightBracket) {
                let element = self.single_expression()?;
                elements.push(element);
                if !self.lookahead_is(TokenType::Comma) {
                    break;
                }
                // Eat the comma
                consume_unchecked!(self);
            }

            let end = consume!(self, TokenType::RightBracket)?;

            return Ok(SingleExpression::Literal(Literal::Array(ArrayLiteral {
                span: start + end,
                elements,
            })));
        };

        self.literal()
    }

    /// Literal
    ///   : boolean
    ///   | number
    ///   | string
    ///   ;
    fn literal(&mut self) -> ParseResult<SingleExpression> {
        let literal: Literal = match self.lookahead_type() {
            Some(TokenType::True) => {
                let span = consume_unchecked!(self);
                BooleanLiteral { span, value: true }.into()
            }
            Some(TokenType::False) => {
                let span = consume_unchecked!(self);
                BooleanLiteral { span, value: false }.into()
            }
            Some(TokenType::String) => {
                let span = consume_unchecked!(self);
                let lexme = span.lexme();
                StringLiteral {
                    span,
                    // Drop quoute characters from value
                    value: &lexme[1..lexme.len() - 1],
                }
                .into()
            }
            Some(TokenType::Integer) => {
                let span = consume_unchecked!(self);
                let lexme = span.lexme();
                IntegerLiteral {
                    span,
                    // Should be safe to unwrap since
                    // the tokenizer matched this
                    value: lexme.parse().unwrap(),
                }
                .into()
            }
            Some(TokenType::HexInteger) => {
                let span = consume_unchecked!(self);
                let without_prefix = span.lexme().trim_start_matches("0x");
                IntegerLiteral {
                    span,
                    // Allow integer overflows in this specific instance
                    value: u32::from_str_radix(without_prefix, 16).unwrap() as i32,
                }
                .into()
            }
            Some(TokenType::Float) => {
                let span = consume_unchecked!(self);
                let lexme = span.lexme();
                FloatingPointLiteral {
                    span,
                    value: lexme.parse::<f32>().unwrap(),
                }
                .into()
            }
            _ => {
                // TODO -rename this error.to something more descriptive
                return Err(ParseError::NoViableAlternative {
                    expected: vec![
                        TokenType::Identifier,
                        TokenType::Integer,
                        TokenType::HexInteger,
                        TokenType::Float,
                        TokenType::String,
                        TokenType::True,
                        TokenType::False,
                    ],
                    actual: self.lookahead_type().unwrap(),
                    span: self.lookahead_span(),
                });
            }
        };

        Ok(SingleExpression::Literal(literal))
    }

    /// TypeAnnotation
    ///   : ':' (PrimitiveType | ObjectType)
    ///   ;
    fn type_annotation(&mut self) -> ParseResult<TypeAnnotation> {
        consume!(self, TokenType::Colon)?;
        let name = ident!(self)?;
        let mut ty = Type::Identifier(IdentifierType {
            name: name.value.clone(),
        });

        let start = name.span();
        let mut end = name.span();
        while self.lookahead_is(TokenType::LeftBracket) {
            consume_unchecked!(self);
            end = consume!(self, TokenType::RightBracket)?;
            ty = Type::Array(ArrayType {
                ident: Box::new(ty),
            })
        }
        Ok(TypeAnnotation {
            ty,
            span: start + end,
        })
    }

    /// Annotation
    ///   : '@' Identifier ('(' SingleExpression ')')?
    ///   ;
    fn annotation(&mut self) -> ParseResult<Annotation> {
        let start = consume!(self, TokenType::At)?;
        let ident = ident!(self)?;

        let mut end = ident.span.to_owned();
        let mut expr = None;
        if self.lookahead_is(TokenType::LeftParen) {
            consume!(self, TokenType::LeftParen)?;
            expr = Some(self.single_expression()?);
            end = consume!(self, TokenType::RightParen)?;
        }

        Ok(Annotation {
            span: start + end,
            name: ident,
            expr,
        })
    }

    /// FormalParameterList
    ///   :  '(' FormalParameterArg ')'
    ///   |  '(' FormalParameterArg , FormalParameterArg ')'
    ///   ;
    fn formal_parameter_list(&mut self) -> ParseResult<FormalParameterList> {
        let start = consume!(self, TokenType::LeftParen)?;
        let mut parameters = vec![];
        if !self.lookahead_is(TokenType::RightParen) {
            loop {
                parameters.push(self.formal_parameter_arg()?);
                if !self.lookahead_is(TokenType::Comma) {
                    break;
                }
                consume_unchecked!(self);
            }
        }
        let end = consume!(self, TokenType::RightParen)?;
        Ok(FormalParameterList {
            span: start + end,
            parameters,
        })
    }

    /// FormalParameterArg
    ///   :  Ident TypeAnnotation
    ///   ;
    fn formal_parameter_arg(&mut self) -> ParseResult<FormalParameterArg> {
        let ident = ident!(self)?;
        let type_annotation = self.type_annotation()?;
        Ok(FormalParameterArg {
            span: ident.span() + type_annotation.span(),
            ident,
            type_annotation,
        })
    }

    /// Return an owned token type value of the
    /// current lookahead token
    fn lookahead_type(&self) -> Option<TokenType> {
        let lookahed = &self.lookahead;
        lookahed.as_ref().map(|token| token.kind)
    }

    /// Checks that the lookahead token is the same
    /// as the given TokenType.
    fn lookahead_is(&self, ty: TokenType) -> bool {
        if let Some(token) = &self.lookahead {
            token.kind == ty
        } else {
            false
        }
    }

    /// Checks that the lookahead token is the same
    /// as the given TokenType. This unsafely unwraps because
    /// It's assumed that all relevant checks have been done first
    fn lookahead_span(&self) -> Span {
        let token = self.lookahead.as_ref().unwrap();
        token.span.clone()
    }

    /// Bail out of the current parse context
    /// by throwing out tokens until we find a new
    /// token that allows us to recover
    fn handle_error_and_recover(&mut self, e: ParseError, recovery_set: &[TokenType]) {
        let mut fixed_with_single_char_insertion = false;
        if let ParseError::MismatchedToken { expected, .. } = &e {
            match expected {
                TokenType::Semi => fixed_with_single_char_insertion = true,
                _ => {
                    fixed_with_single_char_insertion = false;
                }
            }
        };

        self.errors.push(e);

        if fixed_with_single_char_insertion {
            return;
        }

        while self.lookahead.is_some() {
            if recovery_set.contains(&self.lookahead.as_ref().unwrap().kind) {
                break;
            }
            self.lookahead = self.tokenizer.next_token();
        }
    }

    /// Get a reference to the tokenizer's errors.
    pub fn tokenizer_errors(&self) -> Vec<TokenizerError> {
        self.tokenizer.errors()
    }

    /// Get a reference to the parser's errors.
    pub fn parse_errors(&self) -> Vec<ParseError> {
        self.errors.clone()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use jswt_assert::assert_debug_snapshot;

    #[test]
    fn test_parse_empty_program() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_parse_empty_program", "");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_parse_empty_block() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_parse_empty_block", "{}");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_parse_nested_empty_blocks() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_parse_nested_empty_blocks", "{ {} }");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_parse_empty_statement() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_parse_empty_statement", ";");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_parse_variable_statement_with_number() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_parse_variable_statement_with_number", "let x = 42;");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_parse_variable_statement_with_string() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_variable_statement_with_string",
            "let x = \"Hello World\";",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_parse_return_statement() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_parse_return_statement", "return 99;");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_parse_additive_expression() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_parse_additive_expression", "let x = 1 + 2;");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_parse_multiplicative_expression() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_parse_multiplicative_expression", "let x = 3 * 2;");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_parse_nested_math_expression_has_correct_precedence() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_nested_math_expression_has_correct_precedence",
            "let x = 3 * 2 + 1 * 0;",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_parse_additive_expression_left_associativity() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_additive_expression_left_associativity",
            "let x = 3 + 2 + 1 + 0;",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_parse_expression_statement_parses_function_invocation() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_expression_statement_parses_function_invocation",
            "export function main() { test(); }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_parse_argument_expression_with_expression_parameter() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_argument_expression_with_expression_parameter",
            "test(1 + 2);",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_parse_if_statement() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_if_statement",
            "if(x == y) { return 0; } else { return 1; }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_parse_if_else_if_statement() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_if_else_if_statement",
            "if(x == y) { return 0; } else if (x > y) { return 1; } else { return -1; }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_parse_while_iteration_statement() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_while_iteration_statement",
            "while(x < 99) { println(\"hey taco\"); }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_parse_arguments_expression() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_parse_arguments_expression", "print(6);");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_parse_unary_expression() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_parse_unary_expression", "-1 * 10 + -5;");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_parse_array_literal() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_parse_array_literal", "[-1, 10, -5, \"test\"];");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn parse_array_type_annotation() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "parse_array_type_annotation",
            "function exit(code: i32[]): string[] { }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn parse_multi_dimensional_array_type_annotation() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "parse_multi_dimensional_array_type_annotation",
            "function exit(code: i32[][]): string[][] { }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn parse_variable_type_annotation() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("parse_variable_type_annotation", "let x: i32 = 99;");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn parse_postfix_unary_expression() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "parse_postfix_unary_expression",
            "let x: i32 = 99--; let y = i++;",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn parse_this_expression() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("parse_this_expression", "this;");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn parse_member_dot_expression() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "parse_member_dot_expression",
            "function test() { this.index = 10; }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn parse_member_dot_arguments_expression() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "parse_member_dot_arguments_expression",
            "function test() { this.set(10); }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }
}
