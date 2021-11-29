use std::vec;

use crate::errors::{ParseError, TokenizerError};
use crate::tokenizer::{Token, TokenType};
use crate::Tokenizer;
use jswt_ast::*;

/// Returns true if a token matching the given token type
/// was consumed.
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
macro_rules! consume_unchecked {
    ($self:ident) => {{
        let span = $self.lookahead_span();
        $self.lookahead = $self.tokenizer.next_token();
        span
    }};
}

/// Checks that the current lookahead tokentype is the same type
/// as the given token then advances the tokenizer to the next token
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

macro_rules! ident {
    // This macro takes an argument of designator `ident` and
    // creates a function named `$func_name`.
    // The `ident` designator is used for variable/function names.
    ($self:ident) => {{
        let token = $self.lookahead.as_ref().expect("unexpected end of input");
        if token.kind != TokenType::Identifier {
            panic!("Mismatched token type {:?}", token);
        }
        let ident = Ident::from(token);
        // Advance lookahead
        $self.lookahead = $self.tokenizer.next_token();
        ident
    }};
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

    /// Entry point of the program
    ///
    /// Program
    ///   :  SourceElements? Eof
    ///   ;
    fn program(&mut self) -> Program {
        Program {
            // Read until the end of the file
            source_elements: self.source_elements(Some(TokenType::Eof)),
        }
    }

    /// SourceElements
    ///   :  SourceElement
    ///   |  SourceElements SourceElement
    ///   ;
    fn source_elements(&mut self, terminal: Option<TokenType>) -> SourceElements {
        let mut source_elements = vec![];
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
        SourceElements { source_elements }
    }

    /// SourceElement
    ///   :  FunctionDeclaration
    ///   |  Statement
    ///   ;
    fn source_element(&mut self) -> Result<SourceElement, ParseError> {
        let elem = match self.lookahead_type() {
            // Need to check for optional function decorators
            Some(TokenType::Function) | Some(TokenType::Export) | Some(TokenType::At) => {
                self.function_declaration()?.into()
            }
            _ => self.statement()?.into(),
        };
        Ok(elem)
    }

    /// Statement
    ///   :  Block
    ///   |  EmptyStatement
    ///   |  IfStatement
    ///   |  ReturnStatement
    ///   |  VariableStatement
    ///   |  ExpressionStatement
    ///   ;
    fn statement(&mut self) -> Result<StatementElement, ParseError> {
        let elem = match self.lookahead_type() {
            Some(TokenType::LeftBrace) => self.block()?.into(),
            Some(TokenType::Semi) => self.empty_statement()?.into(),
            Some(TokenType::If) => self.if_statement()?.into(),
            Some(TokenType::Return) => self.return_statement()?.into(),
            Some(TokenType::Let) | Some(TokenType::Const) => self.variable_statement()?.into(),
            _ => self.expression_statement()?.into(),
        };
        Ok(elem)
    }

    /// Block
    ///   :  '{' statementList? '}'
    ///   ;
    fn block(&mut self) -> Result<BlockStatement, ParseError> {
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
    fn empty_statement(&mut self) -> Result<EmptyStatement, ParseError> {
        let span = consume!(self, TokenType::Semi)?;
        Ok(EmptyStatement { span })
    }

    /// IfStatement
    ///   : 'if' '(' SingleExpression ')' Statement 'else' Statement
    ///   | 'if' '(' SingleExpression ')' Satement
    ///   ;
    fn if_statement(&mut self) -> Result<IfStatement, ParseError> {
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
            .unwrap_or(consequence.span());

        Ok(IfStatement {
            span: start + end,
            condition,
            consequence,
            alternative,
        })
    }

    /// ReturnStatement
    ///   : 'return' SingleExpression ';'
    ///   ;
    fn return_statement(&mut self) -> Result<ReturnStatement, ParseError> {
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
    fn statement_list(&mut self, terminal: Option<TokenType>) -> Result<StatementList, ParseError> {
        let mut statements = vec![];
        while self.lookahead_type().is_some() && self.lookahead_type() != terminal {
            match self.statement() {
                Ok(element) => statements.push(element),
                Err(err) => self.handle_error_and_recover(
                    err,
                    &[
                        TokenType::LeftBrace,
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
    fn variable_statement(&mut self) -> Result<VariableStatement, ParseError> {
        let modifier = self.variable_modifier()?;
        let target = self.assignable()?;
        consume!(self, TokenType::Equal)?;
        let expression = self.single_expression()?;
        let end = consume!(self, TokenType::Semi)?;

        Ok(VariableStatement {
            span: modifier.span() + end,
            modifier,
            target,
            expression,
        })
    }

    /// VariableModifier
    ///   : 'let'
    ///   | 'const'
    ///   ;
    fn variable_modifier(&mut self) -> Result<VariableModifier, ParseError> {
        let modifier = match self.lookahead_type().unwrap() {
            TokenType::Let => VariableModifier::Let,
            TokenType::Const => VariableModifier::Const,
            // it should never be anything but these
            _ => {
                let lookahead = self.lookahead.as_ref().unwrap();
                return Err(ParseError::NoViableAlternative {
                    expected: vec![TokenType::Let, TokenType::Const],
                    actual: lookahead.kind,
                    span: lookahead.into(),
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
    fn assignable(&mut self) -> Result<AssignableElement, ParseError> {
        Ok(AssignableElement::Identifier(ident!(self)))
    }

    /// ExpressionStatement
    ///   : SingleExpression ';'
    ///   ;
    fn expression_statement(&mut self) -> Result<ExpressionStatement, ParseError> {
        let expression = self.single_expression()?;
        let end = consume!(self, TokenType::Semi)?;

        Ok(ExpressionStatement {
            span: expression.span() + end,
            expression,
        })
    }

    /// SingleExpression
    ///   : SingleExpression Arguments
    ///   | SingleExpression ('*' | '/') SingleExpression
    ///   | SingleExpression ('+' | '-') SingleExpression
    ///   | SingleExpression ('<' | '>' | '<=' | '>=') SingleExpression
    ///   | SingleExpression ('==' | '!=') SingleExpression
    ///   | SingleExpression '&' SingleExpression
    ///   | SingleExpression '|' SingleExpression
    ///   | SingleExpression '=' SingleExpression
    ///   ;
    fn single_expression(&mut self) -> Result<SingleExpression, ParseError> {
        self.assignment_expression()
    }

    /// Expressions are recursively declared with lower precendence items
    /// calling into higher precedence items

    /// AssignmentExpression
    ///   : BitwiseOrExpression '=' BitwiseOrExpression  
    ///   ;
    fn assignment_expression(&mut self) -> Result<SingleExpression, ParseError> {
        let mut left = self.bitwise_and_expression()?;
        while let Some(token) = self.lookahead_type() {
            match token {
                TokenType::Equal => {
                    let op_span = consume_unchecked!(self);
                    let right = self.bitwise_and_expression()?;
                    left = SingleExpression::Bitwise(BinaryExpression {
                        span: left.span() + right.span(),
                        left: Box::new(left),
                        op: BinaryOperator::Assign(op_span),
                        right: Box::new(right),
                    });
                }
                _ => break,
            }
        }
        Ok(left)
    }

    /// BitwiseOrExpression
    ///   : BitwiseAndExpression '|' BitwiseAndExpression
    ///   ;
    fn bitwise_or_expression(&mut self) -> Result<SingleExpression, ParseError> {
        let mut left = self.bitwise_and_expression()?;
        while let Some(token) = self.lookahead_type() {
            match token {
                TokenType::Or => {
                    let op_span = consume_unchecked!(self);
                    let right = self.bitwise_and_expression()?;
                    left = SingleExpression::Bitwise(BinaryExpression {
                        span: left.span() + right.span(),
                        left: Box::new(left),
                        op: BinaryOperator::Or(op_span),
                        right: Box::new(right),
                    });
                }
                _ => break,
            }
        }

        Ok(left)
    }

    /// BitwiseAndExpression
    ///   : EqualityExpression '&' EqualityExpression
    ///   ;
    fn bitwise_and_expression(&mut self) -> Result<SingleExpression, ParseError> {
        let mut left = self.equality_expression()?;
        while let Some(token) = self.lookahead_type() {
            match token {
                TokenType::And => {
                    let op_span = consume_unchecked!(self);
                    let right = self.equality_expression()?;
                    left = SingleExpression::Bitwise(BinaryExpression {
                        span: left.span() + right.span(),
                        left: Box::new(left),
                        op: BinaryOperator::And(op_span),
                        right: Box::new(right),
                    });
                }
                _ => break,
            }
        }

        Ok(left)
    }

    /// EqualityExpression
    ///   : AdditiveExpression ('==' | '!=') AdditiveExpression
    ///   ;
    fn equality_expression(&mut self) -> Result<SingleExpression, ParseError> {
        let mut left = self.relational_expression()?;
        while let Some(token) = self.lookahead_type() {
            match token {
                TokenType::EqualEqual => {
                    let op_span = consume_unchecked!(self);
                    let right = self.relational_expression()?;
                    left = SingleExpression::Equality(BinaryExpression {
                        span: left.span() + right.span(),
                        left: Box::new(left),
                        op: BinaryOperator::Equal(op_span),
                        right: Box::new(right),
                    });
                }
                TokenType::BangEqual => {
                    let op_span = consume_unchecked!(self);
                    let right = self.relational_expression()?;
                    left = SingleExpression::Equality(BinaryExpression {
                        span: left.span() + right.span(),
                        left: Box::new(left),
                        op: BinaryOperator::NotEqual(op_span),
                        right: Box::new(right),
                    });
                }
                _ => break,
            }
        }
        Ok(left)
    }

    /// RelationalExpression
    ///   : SingleExpression ('<' | '>' | '<=' | '>=') SingleExpression
    ///   ;
    fn relational_expression(&mut self) -> Result<SingleExpression, ParseError> {
        let mut left = self.additive_expression()?;
        while let Some(token) = self.lookahead_type() {
            match token {
                TokenType::Greater => {
                    let op_span = consume_unchecked!(self);
                    let right = self.additive_expression()?;
                    left = SingleExpression::Relational(BinaryExpression {
                        span: left.span() + right.span(),
                        left: Box::new(left),
                        op: BinaryOperator::Greater(op_span),
                        right: Box::new(right),
                    });
                }
                TokenType::GreaterEqual => {
                    let op_span = consume_unchecked!(self);
                    let right = self.additive_expression()?;
                    left = SingleExpression::Relational(BinaryExpression {
                        span: left.span() + right.span(),
                        left: Box::new(left),
                        op: BinaryOperator::GreaterEqual(op_span),
                        right: Box::new(right),
                    });
                }
                TokenType::Less => {
                    let op_span = consume_unchecked!(self);
                    let right = self.additive_expression()?;
                    left = SingleExpression::Relational(BinaryExpression {
                        span: left.span() + right.span(),
                        left: Box::new(left),
                        op: BinaryOperator::Less(op_span),
                        right: Box::new(right),
                    });
                }
                TokenType::LessEqual => {
                    let op_span = consume_unchecked!(self);
                    let right = self.additive_expression()?;
                    left = SingleExpression::Relational(BinaryExpression {
                        span: left.span() + right.span(),
                        left: Box::new(left),
                        op: BinaryOperator::LessEqual(op_span),
                        right: Box::new(right),
                    });
                }
                _ => break,
            }
        }
        Ok(left)
    }

    /// AdditiveExpression
    ///   : MultiplicativeExpression ('+' | '-') MultiplicativeExpression
    ///   ;
    fn additive_expression(&mut self) -> Result<SingleExpression, ParseError> {
        // This gives multiplicative expressions higher precedence
        let mut left = self.multipicative_expression()?;
        while let Some(token) = self.lookahead_type() {
            match token {
                TokenType::Plus => {
                    let op_span = consume_unchecked!(self);
                    let right = self.multipicative_expression()?;
                    left = SingleExpression::Additive(BinaryExpression {
                        span: left.span() + right.span(),
                        left: Box::new(left),
                        op: BinaryOperator::Plus(op_span),
                        right: Box::new(right),
                    });
                }
                TokenType::Minus => {
                    let op_span = consume_unchecked!(self);
                    let right = self.multipicative_expression()?;
                    left = SingleExpression::Additive(BinaryExpression {
                        span: left.span() + right.span(),
                        left: Box::new(left),
                        op: BinaryOperator::Minus(op_span),
                        right: Box::new(right),
                    });
                }
                _ => break,
            }
        }
        Ok(left)
    }

    /// MultiplicativeExpression
    ///   : IdentifierExpression ('*' | '/' | '%') IdentifierExpression
    ///   ;
    fn multipicative_expression(&mut self) -> Result<SingleExpression, ParseError> {
        let mut left = self.arguments_expression()?;
        while let Some(token) = self.lookahead_type() {
            match token {
                TokenType::Star => {
                    let op_span = consume_unchecked!(self);
                    let right = self.arguments_expression()?;
                    left = SingleExpression::Multiplicative(BinaryExpression {
                        span: left.span() + right.span(),
                        left: Box::new(left),
                        op: BinaryOperator::Star(op_span),
                        right: Box::new(right),
                    });
                }
                TokenType::Slash => {
                    let op_span = consume_unchecked!(self);
                    let right = self.identifier_expression()?;
                    left = SingleExpression::Multiplicative(BinaryExpression {
                        span: left.span() + right.span(),
                        left: Box::new(left),
                        op: BinaryOperator::Slash(op_span),
                        right: Box::new(right),
                    });
                }
                _ => break,
            }
        }
        Ok(left)
    }

    /// IdentifierExpression
    ///   : Identifier
    ///   ;
    fn identifier_expression(&mut self) -> Result<SingleExpression, ParseError> {
        if self.lookahead_is(TokenType::Identifier) {
            return Ok(SingleExpression::Identifier(IdentifierExpression {
                ident: ident!(self),
            }));
        }
        // If we can't find an ident, continue by trying to resolve a literal
        self.literal()
    }

    /// ArgumentsExpression
    ///   :  AdditiveExpression ArgumentList
    ///   ;
    ///
    fn arguments_expression(&mut self) -> Result<SingleExpression, ParseError> {
        // Eventually descend to ident
        let left = self.identifier_expression()?;
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
    fn argument_list(&mut self) -> Result<ArgumentsList, ParseError> {
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

    /// Literal
    ///   : boolean
    ///   | number
    ///   | string
    ///   ;
    fn literal(&mut self) -> Result<SingleExpression, ParseError> {
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
                let value = self.lookahead.as_ref().unwrap().lexme;
                let span = consume_unchecked!(self);
                StringLiteral {
                    span,
                    // Drop quoute characters from value
                    value: &value[1..value.len() - 1],
                }
                .into()
            }
            Some(TokenType::Number) => {
                let inner = self.lookahead.as_ref().unwrap().lexme;
                let span = consume_unchecked!(self);
                NumberLiteral {
                    span,
                    // Should be safe to unwrap since
                    // the tokenizer matched this
                    value: inner.parse().unwrap(),
                }
                .into()
            }
            _ => {
                // TODO -rename this error.to something more descriptive
                return Err(ParseError::NoViableAlternative {
                    expected: vec![
                        TokenType::Identifier,
                        TokenType::Number,
                        TokenType::String,
                        TokenType::True,
                        TokenType::False,
                    ],
                    actual: self.lookahead_type().unwrap(),
                    span: self.lookahead_span(),
                });
            }
        };
        Ok(literal.into())
    }

    /// FunctionDeclaration
    ///   :  Annotation? 'export'? 'function' Identifier ( FormalParameterList? ) TypeAnnotation? FunctionBody
    ///   ;
    fn function_declaration(&mut self) -> Result<FunctionDeclarationElement, ParseError> {
        let mut annotation = None;
        if self.lookahead_is(TokenType::At) {
            annotation = Some(self.annotation()?);
        }

        let export_span = maybe_consume!(self, TokenType::Export);
        let function_span = consume!(self, TokenType::Function)?;
        let start_span = export_span.to_owned().unwrap_or(function_span);

        let ident = ident!(self);

        consume!(self, TokenType::LeftParen)?;
        let params = self.formal_parameter_list()?;
        consume!(self, TokenType::RightParen)?;

        //Parse return value
        let mut returns = None;
        if self.lookahead_is(TokenType::Colon) {
            returns = Some(self.type_annotation()?);
        }

        let body = self.function_body()?;

        let decorators = FunctionDecorators {
            annotation,
            export: export_span.is_some(),
        };
        Ok(FunctionDeclarationElement {
            span: start_span + body.span(),
            decorators,
            ident,
            params,
            returns,
            body,
        })
    }

    fn annotation(&mut self) -> Result<Annotation, ParseError> {
        let start = consume!(self, TokenType::At)?;
        let ident = ident!(self);
        consume!(self, TokenType::LeftParen)?;
        let expr = self.single_expression()?;
        let end = consume!(self, TokenType::RightParen)?;

        Ok(Annotation {
            span: start + end,
            name: ident,
            expr,
        })
    }

    /// FormalParameterList
    ///   :  FormalParameterArg
    ///   |  FormalParameterArg , FormalParameterArg
    ///   ;
    fn formal_parameter_list(&mut self) -> Result<FormalParameterList, ParseError> {
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
        Ok(FormalParameterList { parameters })
    }

    /// FormalParameterArg
    ///   :  Ident TypeAnnotation
    ///   ;
    fn formal_parameter_arg(&mut self) -> Result<FormalParameterArg, ParseError> {
        let ident = ident!(self);
        let type_annotation = self.type_annotation()?;
        Ok(FormalParameterArg {
            ident,
            type_annotation,
        })
    }

    /// TypeAnnotation
    ///   : ':' Ident
    ///   ;
    fn type_annotation(&mut self) -> Result<Ident, ParseError> {
        consume!(self, TokenType::Colon)?;
        let type_param = ident!(self);
        Ok(type_param)
    }

    ///  FunctionBody
    ///    :  '{' SourceElements? '}'
    ///    ;
    fn function_body(&mut self) -> Result<FunctionBody, ParseError> {
        let start = consume!(self, TokenType::LeftBrace)?;
        // Read until we find a closing brace
        let source_elements = self.source_elements(Some(TokenType::RightBrace));
        let end = consume!(self, TokenType::RightBrace)?;
        Ok(FunctionBody {
            span: start + end,
            source_elements,
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
        self.lookahead.as_ref().unwrap().into()
    }

    /// Bail out of the current parse context
    /// by throwing out tokens until we find a new
    /// token that allows us to recover
    fn handle_error_and_recover(&mut self, e: ParseError, recovery_set: &[TokenType]) {
        self.errors.push(e);
        while self.lookahead.is_some() {
            if recovery_set.contains(&self.lookahead.as_ref().unwrap().kind) {
                break;
            }
            self.lookahead = self.tokenizer.next_token();
        }
    }

    /// Get a reference to the parser's errors.
    pub fn errors(&self) -> (Vec<ParseError>, Vec<TokenizerError>) {
        (self.errors.clone(), self.tokenizer.errors())
    }
}

impl From<&Token> for Ident {
    fn from(token: &Token) -> Self {
        Ident::new(token.lexme, token.into())
    }
}

impl From<&Token> for Span {
    fn from(token: &Token) -> Self {
        Span::new(&token.file, token.offset, token.offset + token.lexme.len())
    }
}

#[cfg(test)]
mod test {
    use std::vec;

    use super::*;
    use crate::{assert_str_eq, Tokenizer};
    use pretty_assertions::assert_eq;

    #[test]
    fn test_function_declaration_statement() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "function test() { }");
        let actual = Parser::new(&mut tokenizer).parse().program;
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![SourceElement::FunctionDeclaration(
                    FunctionDeclarationElement {
                        span: Span::new("test.1", 0, 19),
                        decorators: FunctionDecorators {
                            annotation: None,
                            export: false,
                        },
                        ident: Ident {
                            span: Span::new("test.1", 9, 13),
                            value: "test",
                        },
                        params: FormalParameterList { parameters: vec![] },
                        returns: None,
                        body: FunctionBody {
                            span: Span::new("test.1", 16, 19),
                            source_elements: SourceElements {
                                source_elements: vec![],
                            },
                        },
                    },
                )],
            },
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_empty_program() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "");
        let actual = Parser::new(&mut tokenizer).parse().program;
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![],
            },
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_function_declaration_statement_with_one_param() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "function name(a: i32) { }");
        let actual = Parser::new(&mut tokenizer).parse().program;
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![SourceElement::FunctionDeclaration(
                    FunctionDeclarationElement {
                        span: Span::new("test.1", 0, 25),
                        decorators: FunctionDecorators {
                            annotation: None,
                            export: false,
                        },
                        ident: Ident {
                            span: Span::new("test.1", 9, 13),
                            value: "name",
                        },
                        params: FormalParameterList {
                            parameters: vec![FormalParameterArg {
                                ident: Ident {
                                    span: Span::new("test.1", 14, 15),
                                    value: "a",
                                },
                                type_annotation: Ident {
                                    span: Span::new("test.1", 17, 20),
                                    value: "i32",
                                },
                            }],
                        },
                        returns: None,
                        body: FunctionBody {
                            span: Span::new("test.1", 22, 25),
                            source_elements: SourceElements {
                                source_elements: vec![],
                            },
                        },
                    },
                )],
            },
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_function_declaration_statement_with_two_params() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "function name(a: i32, b: f32) { }");
        let actual = Parser::new(&mut tokenizer).parse().program;
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![SourceElement::FunctionDeclaration(
                    FunctionDeclarationElement {
                        span: Span::new("test.1", 0, 33),
                        decorators: FunctionDecorators {
                            annotation: None,
                            export: false,
                        },
                        ident: Ident {
                            span: Span {
                                file: "test.1".to_owned(),
                                start: 9,
                                end: 13,
                            },
                            value: "name",
                        },
                        params: FormalParameterList {
                            parameters: vec![
                                FormalParameterArg {
                                    ident: Ident {
                                        span: Span {
                                            file: "test.1".to_owned(),
                                            start: 14,
                                            end: 15,
                                        },
                                        value: "a",
                                    },
                                    type_annotation: Ident {
                                        span: Span {
                                            file: "test.1".to_owned(),
                                            start: 17,
                                            end: 20,
                                        },
                                        value: "i32",
                                    },
                                },
                                FormalParameterArg {
                                    ident: Ident {
                                        span: Span {
                                            file: "test.1".to_owned(),
                                            start: 22,
                                            end: 23,
                                        },
                                        value: "b",
                                    },
                                    type_annotation: Ident {
                                        span: Span {
                                            file: "test.1".to_owned(),
                                            start: 25,
                                            end: 28,
                                        },
                                        value: "f32",
                                    },
                                },
                            ],
                        },
                        returns: None,
                        body: FunctionBody {
                            span: Span::new("test.1", 30, 33),
                            source_elements: SourceElements {
                                source_elements: vec![],
                            },
                        },
                    },
                )],
            },
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_function_declaration_statement_with_export_decorator() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "export function test() { }");
        let actual = Parser::new(&mut tokenizer).parse().program;
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![SourceElement::FunctionDeclaration(
                    FunctionDeclarationElement {
                        span: Span::new("test.1", 0, 26),
                        decorators: FunctionDecorators {
                            annotation: None,
                            export: true,
                        },
                        ident: Ident {
                            span: Span {
                                file: "test.1".to_owned(),
                                start: 16,
                                end: 20,
                            },
                            value: "test",
                        },
                        params: FormalParameterList { parameters: vec![] },
                        returns: None,
                        body: FunctionBody {
                            span: Span::new("test.1", 23, 26),
                            source_elements: SourceElements {
                                source_elements: vec![],
                            },
                        },
                    },
                )],
            },
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_function_declaration_statement_with_return_value() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "function test(): i32 { }");
        let actual = Parser::new(&mut tokenizer).parse().program;
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![SourceElement::FunctionDeclaration(
                    FunctionDeclarationElement {
                        span: Span::new("test.1", 0, 24),
                        decorators: FunctionDecorators {
                            annotation: None,
                            export: false,
                        },
                        ident: Ident {
                            span: Span {
                                file: "test.1".to_owned(),
                                start: 9,
                                end: 13,
                            },
                            value: "test",
                        },
                        params: FormalParameterList { parameters: vec![] },
                        returns: Some(Ident {
                            span: Span {
                                file: "test.1".to_owned(),
                                start: 17,
                                end: 20,
                            },
                            value: "i32",
                        }),
                        body: FunctionBody {
                            span: Span::new("test.1", 21, 24),
                            source_elements: SourceElements {
                                source_elements: vec![],
                            },
                        },
                    },
                )],
            },
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_function_declaration_statement_with_two_params_and_return_value() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "function test(a: i32, b: i32): i32 { }");
        let actual = Parser::new(&mut tokenizer).parse().program;
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![SourceElement::FunctionDeclaration(
                    FunctionDeclarationElement {
                        span: Span::new("test.1", 0, 38),
                        decorators: FunctionDecorators {
                            annotation: None,
                            export: false,
                        },
                        ident: Ident {
                            span: Span::new("test.1", 9, 13),
                            value: "test",
                        },
                        params: FormalParameterList {
                            parameters: vec![
                                FormalParameterArg {
                                    ident: Ident {
                                        span: Span::new("test.1", 14, 15),

                                        value: "a",
                                    },
                                    type_annotation: Ident {
                                        span: Span::new("test.1", 17, 20),
                                        value: "i32",
                                    },
                                },
                                FormalParameterArg {
                                    ident: Ident {
                                        span: Span::new("test.1", 22, 23),
                                        value: "b",
                                    },
                                    type_annotation: Ident {
                                        span: Span::new("test.1", 25, 28),
                                        value: "i32",
                                    },
                                },
                            ],
                        },
                        returns: Some(Ident {
                            span: Span::new("test.1", 31, 34),
                            value: "i32",
                        }),
                        body: FunctionBody {
                            span: Span::new("test.1", 35, 38),
                            source_elements: SourceElements {
                                source_elements: vec![],
                            },
                        },
                    },
                )],
            },
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_empty_block() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "{}");
        let actual = Parser::new(&mut tokenizer).parse().program;
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![SourceElement::Statement(StatementElement::Block(
                    BlockStatement {
                        span: Span::new("test.1", 0, 2),
                        statements: StatementList { statements: vec![] },
                    },
                ))],
            },
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_nested_empty_blocks() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "{ {} }");
        let actual = Parser::new(&mut tokenizer).parse().program;
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![SourceElement::Statement(StatementElement::Block(
                    BlockStatement {
                        span: Span::new("test.1", 0, 6),
                        statements: StatementList {
                            statements: vec![StatementElement::Block(BlockStatement {
                                span: Span::new("test.1", 2, 4),
                                statements: StatementList { statements: vec![] },
                            })],
                        },
                    },
                ))],
            },
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_function_declaration_statement_with_block_body() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "function test() { {} }");
        let actual = Parser::new(&mut tokenizer).parse().program;
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![SourceElement::FunctionDeclaration(
                    FunctionDeclarationElement {
                        span: Span::new("test.1", 0, 22),
                        decorators: FunctionDecorators {
                            annotation: None,
                            export: false,
                        },
                        ident: Ident {
                            span: Span {
                                file: "test.1".to_owned(),
                                start: 9,
                                end: 13,
                            },
                            value: "test",
                        },
                        params: FormalParameterList { parameters: vec![] },
                        returns: None,
                        body: FunctionBody {
                            span: Span::new("test.1", 16, 22),
                            source_elements: SourceElements {
                                source_elements: vec![SourceElement::Statement(
                                    StatementElement::Block(BlockStatement {
                                        span: Span::new("test.1", 18, 20),
                                        statements: StatementList { statements: vec![] },
                                    }),
                                )],
                            },
                        },
                    },
                )],
            },
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_empty_statement() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", ";");
        let actual = Parser::new(&mut tokenizer).parse().program;
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![SourceElement::Statement(StatementElement::Empty(
                    EmptyStatement {
                        span: Span::new("test.1", 0, 1),
                    },
                ))],
            },
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_variable_statement_with_number() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "let x = 42;");

        let actual = Parser::new(&mut tokenizer).parse().program;
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![SourceElement::Statement(StatementElement::Variable(
                    VariableStatement {
                        span: Span::new("test.1", 0, 11),
                        modifier: VariableModifier::Let(Span::new("test.1", 0, 3)),
                        target: AssignableElement::Identifier(Ident {
                            span: Span::new("test.1", 4, 5),
                            value: "x",
                        }),
                        expression: SingleExpression::Literal(Literal::Number(NumberLiteral {
                            span: Span::new("test.1", 8, 10),
                            value: 42,
                        })),
                    },
                ))],
            },
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_variable_statement_with_string() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "let x = \"Hello World\";");
        let actual = Parser::new(&mut tokenizer).parse().program;
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![SourceElement::Statement(StatementElement::Variable(
                    VariableStatement {
                        span: Span::new("test.1", 0, 22),
                        modifier: VariableModifier::Let(Span::new("test.1", 0, 3)),
                        target: AssignableElement::Identifier(Ident {
                            span: Span {
                                file: "test.1".to_owned(),
                                start: 4,
                                end: 5,
                            },
                            value: "x",
                        }),
                        expression: SingleExpression::Literal(Literal::String(StringLiteral {
                            span: Span::new("test.1", 8, 21),
                            value: "Hello World",
                        })),
                    },
                ))],
            },
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_return_statement() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "return 99;");
        let actual = Parser::new(&mut tokenizer).parse().program;
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![SourceElement::Statement(StatementElement::Return(
                    ReturnStatement {
                        span: Span::new("test.1", 0, 10),
                        expression: SingleExpression::Literal(Literal::Number(NumberLiteral {
                            span: Span::new("test.1", 7, 9),
                            value: 99,
                        })),
                    },
                ))],
            },
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_additive_expression() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "let x = 1 + 2;");
        let actual = Parser::new(&mut tokenizer).parse().program;
        let expected = include_str!("../../test/test_parse_additive_expression.ast");
        assert_str_eq!(expected, &format!("{:#?}", actual));
    }

    #[test]
    fn test_parse_multiplicative_expression() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "let x = 3 * 2;");
        let actual = Parser::new(&mut tokenizer).parse().program;
        let expected = include_str!("../../test/test_parse_multiplicative_expression.ast");
        assert_str_eq!(expected, &format!("{:#?}", actual));
    }

    #[test]
    fn test_parse_nested_math_expression_has_correct_precedence() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "let x = 3 * 2 + 1 * 0;");
        let actual = Parser::new(&mut tokenizer).parse().program;
        let expected =
            include_str!("../../test/test_parse_nested_math_expression_has_correct_precedence.ast");
        assert_str_eq!(expected, &format!("{:#?}", actual));
    }

    #[test]
    fn test_parse_additive_expression_left_associativity() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "let x = 3 + 2 + 1 + 0;");
        let actual = Parser::new(&mut tokenizer).parse().program;
        let expected =
            include_str!("../../test/test_parse_additive_expression_left_associativity.ast");
        assert_str_eq!(expected, &format!("{:#?}", actual));
    }

    #[test]
    fn test_parse_expression_statement_parses_function_invocation() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "export function main() { test(); }");
        let actual = Parser::new(&mut tokenizer).parse();
        let expected = include_str!(
            "../../test/test_parse_expression_statement_parses_function_invocation.ast"
        );
        assert_str_eq!(expected, &format!("{:#?}", actual));
    }

    #[test]
    fn test_parse_argument_expression_with_expression_parameter() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "test(1 + 2);");
        let actual = Parser::new(&mut tokenizer).parse();
        let expected =
            include_str!("../../test/test_parse_argument_expression_with_expression_parameter.ast");
        assert_str_eq!(expected, &format!("{:#?}", actual));
    }
}
