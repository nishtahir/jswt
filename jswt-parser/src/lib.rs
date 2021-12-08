mod errors;
use std::vec;

pub use errors::ParseError;
use jswt_ast::*;
use jswt_tokenizer::*;

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

        let span = Span::new(&token.file, token.offset, token.offset + token.lexme.len());
        let ident = Ident::new(token.lexme, span);
        // Advance lookahead
        $self.lookahead = $self.tokenizer.next_token();
        ident
    }};
}

macro_rules! binary_expression {
    ($name:ident, next: $next:ident, $([exp => $exp:ident, op => $op:ident, token => $token:ident]),*) => {
        fn $name(&mut self) -> Result<SingleExpression, ParseError> {
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

    /// Entry point of the program
    ///
    /// Program
    ///   :  SourceElements? Eof
    ///   ;
    fn program(&mut self) -> Program {
        Program {
            // Read until the end of the file
            source_elements: self.source_elements(None),
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
    ///   |  IterationStatement
    ///   |  ReturnStatement
    ///   |  VariableStatement
    ///   |  ExpressionStatement
    ///   ;
    fn statement(&mut self) -> Result<StatementElement, ParseError> {
        let elem = match self.lookahead_type() {
            Some(TokenType::LeftBrace) => self.block()?,
            Some(TokenType::Semi) => self.empty_statement()?,
            Some(TokenType::If) => self.if_statement()?,
            Some(TokenType::While) => self.iteration_statement()?,
            Some(TokenType::Return) => self.return_statement()?,
            Some(TokenType::Let) | Some(TokenType::Const) => self.variable_statement()?,
            _ => self.expression_statement()?,
        };
        Ok(elem)
    }

    /// Block
    ///   :  '{' statementList? '}'
    ///   ;
    fn block(&mut self) -> Result<StatementElement, ParseError> {
        let start = consume!(self, TokenType::LeftBrace)?;
        let statements = self.statement_list(Some(TokenType::RightBrace))?;
        let end = consume!(self, TokenType::RightBrace)?;
        Ok(BlockStatement {
            span: start + end,
            statements,
        }
        .into())
    }

    /// EmptyStatement
    ///   : ';'
    ///   ;
    fn empty_statement(&mut self) -> Result<StatementElement, ParseError> {
        let span = consume!(self, TokenType::Semi)?;
        Ok(EmptyStatement { span }.into())
    }

    /// IfStatement
    ///   : 'if' '(' SingleExpression ')' Statement 'else' Statement
    ///   | 'if' '(' SingleExpression ')' Satement
    ///   ;
    fn if_statement(&mut self) -> Result<StatementElement, ParseError> {
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
        }
        .into())
    }

    /// IterationStatement
    ///   :  WhileStatement
    ///   ;
    fn iteration_statement(&mut self) -> Result<StatementElement, ParseError> {
        let elem = match self.lookahead_type() {
            Some(TokenType::While) => self.while_statement()?,
            _ => todo!(),
        };

        Ok(elem.into())
    }

    /// WhileStatement
    ///   : 'while' '(' SingleExpression ')' Statement
    ///   ;
    fn while_statement(&mut self) -> Result<IterationStatement, ParseError> {
        let start = consume!(self, TokenType::While)?;
        consume!(self, TokenType::LeftParen)?;
        let expression = self.single_expression()?;
        consume!(self, TokenType::RightParen)?;
        let statement = self.statement()?;

        Ok(WhileIterationElement {
            span: start + statement.span(),
            expression,
            statement: Box::new(statement),
        }
        .into())
    }

    /// ReturnStatement
    ///   : 'return' SingleExpression ';'
    ///   ;
    fn return_statement(&mut self) -> Result<StatementElement, ParseError> {
        let start = consume!(self, TokenType::Return)?;
        let expression = self.single_expression()?;
        let end = consume!(self, TokenType::Semi)?;

        Ok(ReturnStatement {
            span: start + end,
            expression,
        }
        .into())
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
    fn variable_statement(&mut self) -> Result<StatementElement, ParseError> {
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
        }
        .into())
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
                    span: Span::new(
                        &lookahead.file,
                        lookahead.offset,
                        lookahead.offset + lookahead.lexme.len(),
                    ),
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
    fn expression_statement(&mut self) -> Result<StatementElement, ParseError> {
        let expression = self.single_expression()?;
        let end = consume!(self, TokenType::Semi)?;

        Ok(ExpressionStatement {
            span: expression.span() + end,
            expression,
        }
        .into())
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

    //  AssignmentExpression
    //    : BitwiseOrExpression '=' BitwiseOrExpression
    //    ;
    binary_expression!(
        assignment_expression,
        next: bitwise_or_expression,
        [exp => Assignment, op => Assign, token => Equal]
    );

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
    //    : IdentifierExpression ('*' | '/' | '%') IdentifierExpression
    //    ;
    binary_expression!(
        multipicative_expression,
        next: arguments_expression,
        [exp => Multiplicative, op => Mult, token => Star],
        [exp => Multiplicative, op => Div, token => Slash]
    );

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

    /// IdentifierExpression
    ///   : Identifier
    ///   ;
    fn identifier_expression(&mut self) -> Result<SingleExpression, ParseError> {
        if self.lookahead_is(TokenType::Identifier) {
            let ident = ident!(self);
            return Ok(SingleExpression::Identifier(IdentifierExpression {
                span: ident.span.to_owned(),
                ident,
            }));
        }
        // If we can't find an ident, continue by trying to resolve a literal
        self.literal()
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
            Some(TokenType::HexNumber) => {
                let inner = self.lookahead.as_ref().unwrap().lexme;
                let without_prefix = inner.trim_start_matches("0x");
                let span = consume_unchecked!(self);
                NumberLiteral {
                    span,
                    // Allow integer overflows in this specific instance
                    value: u32::from_str_radix(without_prefix, 16).unwrap() as i32,
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

        Ok(SingleExpression::Literal(literal))
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
        let token = self.lookahead.as_ref().unwrap();
        Span::new(&token.file, token.offset, token.offset + token.lexme.len())
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

#[cfg(test)]
mod test {
    use super::*;
    use jswt_assert::assert_debug_snapshot;

    #[test]
    fn test_function_declaration_statement() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "function test() { }");
        let actual = Parser::new(&mut tokenizer).parse();
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_parse_empty_program() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "");
        let actual = Parser::new(&mut tokenizer).parse();
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_function_declaration_statement_with_one_param() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "function name(a: i32) { }");
        let actual = Parser::new(&mut tokenizer).parse();
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_function_declaration_statement_with_two_params() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "function name(a: i32, b: f32) { }");
        let actual = Parser::new(&mut tokenizer).parse();
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_function_declaration_statement_with_export_decorator() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "export function test() { }");
        let actual = Parser::new(&mut tokenizer).parse();
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_function_declaration_statement_with_return_value() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "function test(): i32 { }");
        let actual = Parser::new(&mut tokenizer).parse();
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_parse_function_declaration_statement_with_two_params_and_return_value() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "function test(a: i32, b: i32): i32 { }");
        let actual = Parser::new(&mut tokenizer).parse();
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_parse_empty_block() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "{}");
        let actual = Parser::new(&mut tokenizer).parse();
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_parse_nested_empty_blocks() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "{ {} }");
        let actual = Parser::new(&mut tokenizer).parse();
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_function_declaration_statement_with_block_body() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "function test() { {} }");
        let actual = Parser::new(&mut tokenizer).parse();
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_parse_empty_statement() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", ";");
        let actual = Parser::new(&mut tokenizer).parse();
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_parse_variable_statement_with_number() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "let x = 42;");
        let actual = Parser::new(&mut tokenizer).parse();
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_parse_variable_statement_with_string() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "let x = \"Hello World\";");
        let actual = Parser::new(&mut tokenizer).parse();
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_parse_return_statement() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "return 99;");
        let actual = Parser::new(&mut tokenizer).parse();
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_parse_additive_expression() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "let x = 1 + 2;");
        let actual = Parser::new(&mut tokenizer).parse();
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_parse_multiplicative_expression() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "let x = 3 * 2;");
        let actual = Parser::new(&mut tokenizer).parse();
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_parse_nested_math_expression_has_correct_precedence() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "let x = 3 * 2 + 1 * 0;");
        let actual = Parser::new(&mut tokenizer).parse();
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_parse_additive_expression_left_associativity() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "let x = 3 + 2 + 1 + 0;");
        let actual = Parser::new(&mut tokenizer).parse();
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_parse_expression_statement_parses_function_invocation() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "export function main() { test(); }");
        let actual = Parser::new(&mut tokenizer).parse();
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_parse_argument_expression_with_expression_parameter() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "test(1 + 2);");
        let actual = Parser::new(&mut tokenizer).parse();
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_parse_if_statement() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "if(x == y) { return 0; } else { return 1; }");
        let actual = Parser::new(&mut tokenizer).parse();
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_parse_if_else_if_statement() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str(
            "test.1",
            "if(x == y) { return 0; } else if (x > y) { return 1; } else { return -1; }",
        );
        let actual = Parser::new(&mut tokenizer).parse();
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_parse_while_iteration_statement() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "while(x < 99) { println(\"hey taco\"); }");
        let actual = Parser::new(&mut tokenizer).parse();
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_parse_arguments_expression() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "print(6);");
        let actual = Parser::new(&mut tokenizer).parse();
        assert_debug_snapshot!(actual);
    }
}
