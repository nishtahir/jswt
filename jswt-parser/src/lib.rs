mod class;
mod errors;
mod expression;
mod function;
mod import;
mod literal;
mod program;
mod statement;
mod variable;

pub use errors::ParseError;
use std::vec;

use jswt_ast::*;
use jswt_common::{Span, Spannable, Type};
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

    //// StatementList
    ////   :  Statement
    ////   |  StatementList Statement
    ////   ;
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

    //// Assignable
    ////   : Ident
    ////   ;
    fn assignable(&mut self) -> ParseResult<AssignableElement> {
        Ok(AssignableElement::Identifier(ident!(self)?))
    }

    //// TypeAnnotation
    ////   : ':' (PrimitiveType | ObjectType)
    ////   ;
    fn type_annotation(&mut self) -> ParseResult<TypeAnnotation> {
        consume!(self, TokenType::Colon)?;
        let name = ident!(self)?;
        let mut ty = Type::Binding(name.value.clone());

        let start = name.span();
        let mut end = name.span();
        while self.lookahead_is(TokenType::LeftBracket) {
            consume_unchecked!(self);
            end = consume!(self, TokenType::RightBracket)?;
            // TODO
            // ty = Type::Binding(ArrayType {
            //     ident: Box::new(ty),
            // })
            ty = Type::Unknown;
        }
        Ok(TypeAnnotation {
            ty,
            span: start + end,
        })
    }

    //// AnnotationList
    ////  : Annotation
    //// | AnnotationList Annotation
    //// ;
    // fn annotation_list(&mut self) -> ParseResult<AnnotationList> {
    //     let mut annotations = vec![];
    //     while self.lookahead_is(TokenType::At) {
    //         annotations.push(self.annotation()?);
    //     }
    //     Ok(AnnotationList { annotations })
    // }

    //// Annotation
    ////   : '@' Identifier ('(' SingleExpression ')')?
    ////   ;
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

    //// FormalParameterList
    ////   :  '(' FormalParameterArg ')'
    ////   |  '(' FormalParameterArg , FormalParameterArg ')'
    ////   ;
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

    //// FormalParameterArg
    ////   :  Ident TypeAnnotation
    ////   ;
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
        tokenizer.enqueue_source_str(
            "test_parse_return_statement",
            "function test() { return 99; }",
        );
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
    fn test_parse_while_iteration_statement() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_while_iteration_statement",
            "function main() { while(x < 99) { println(\"hey taco\"); } }",
        );
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
}
