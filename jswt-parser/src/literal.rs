use jswt_ast::{
    BooleanLiteral, FloatingPointLiteral, IntegerLiteral, Literal, SingleExpression, StringLiteral,
};
use jswt_common::Type;
use jswt_tokenizer::TokenType;

use crate::{consume_unchecked, ParseError, ParseResult, Parser};

impl<'a> Parser<'a> {
    ////
    //// Literal
    ////   : boolean
    ////   | number
    ////   | string
    ////   ;
    ////
    pub(crate) fn literal(&mut self) -> ParseResult<SingleExpression> {
        let literal: Literal = match self.lookahead_type() {
            Some(TokenType::True) => {
                let span = consume_unchecked!(self);
                BooleanLiteral {
                    span,
                    value: true,
                    ty: Type::BOOLEAN,
                }
                .into()
            }
            Some(TokenType::False) => {
                let span = consume_unchecked!(self);
                BooleanLiteral {
                    span,
                    value: false,
                    ty: Type::BOOLEAN,
                }
                .into()
            }
            Some(TokenType::String) => {
                let span = consume_unchecked!(self);
                let lexme = span.lexme();
                StringLiteral {
                    span,
                    // Drop quoute characters from value
                    value: &lexme[1..lexme.len() - 1],
                    ty: Type::STRING,
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
                    ty: Type::I32,
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
                    ty: Type::I32,
                }
                .into()
            }
            Some(TokenType::Float) => {
                let span = consume_unchecked!(self);
                let lexme = span.lexme();
                FloatingPointLiteral {
                    span,
                    value: lexme.parse::<f32>().unwrap(),
                    ty: Type::F32,
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
}

#[cfg(test)]
mod test {

    use super::*;
    use jswt_assert::assert_debug_snapshot;
    use jswt_tokenizer::Tokenizer;

    #[test]
    fn test_boolean_literals() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_boolean_literals", "function test() { true; false; }");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0, "{:#?}", parser.errors);
    }

    #[test]
    fn test_integer_literals() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_integer_literals", "function test() { 1; 0x1; }");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0, "{:#?}", parser.errors);
    }

    #[test]
    fn test_float_literals() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_float_literals", "function test() { 1.0; }");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0, "{:#?}", parser.errors);
    }

    #[test]
    fn test_string_literals() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_string_literals", r#"function test() { "hello"; }"#);
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0, "{:#?}", parser.errors);
    }
}
