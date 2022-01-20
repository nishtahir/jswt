use jswt_ast::*;
use jswt_common::{Span, Spannable};
use jswt_tokenizer::TokenType;

use crate::{consume, consume_unchecked, ident, maybe_consume};
use crate::{ParseError, ParseResult, Parser};

impl<'a> Parser<'a> {
    /// FunctionDeclaration
    ///   :  Annotation? 'export'? 'function' Identifier ( FormalParameterList? ) TypeAnnotation? FunctionBody
    ///   ;
    pub(crate) fn function_declaration(&mut self) -> ParseResult<FunctionDeclarationElement> {
        let mut annotations = vec![];
        while self.lookahead_is(TokenType::At) {
            annotations.push(self.annotation()?);
        }

        let export_span = maybe_consume!(self, TokenType::Export);
        let function_span = consume!(self, TokenType::Function)?;
        let start_span = export_span.to_owned().unwrap_or(function_span);

        let ident = ident!(self)?;

        consume!(self, TokenType::LeftParen)?;
        let params = self.formal_parameter_list()?;
        consume!(self, TokenType::RightParen)?;

        //Parse return value
        let mut returns = None;
        if self.lookahead_is(TokenType::Colon) {
            returns = Some(self.type_annotation()?);
        }

        let body = self.block()?;

        let decorators = FunctionDecorators {
            annotations,
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
}

#[cfg(test)]
mod test {
    use super::*;
    use jswt_assert::assert_debug_snapshot;
    use jswt_tokenizer::Tokenizer;

    #[test]
    fn test_function_declaration_statement() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test.1", "function test() { }");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_function_declaration_statement_with_one_param() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test.1", "function name(a: i32) { }");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_function_declaration_statement_with_two_params() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test.1", "function name(a: i32, b: f32) { }");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_function_declaration_statement_with_export_decorator() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test.1", "export function test() { }");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_function_declaration_statement_with_return_value() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test.1", "function test(): i32 { }");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_parse_function_declaration_statement_with_two_params_and_return_value() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test.1", "function test(a: i32, b: i32): i32 { }");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_function_declaration_statement_with_block_body() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test.1", "function test() { {} }");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_parse_function_with_multiple_annotations() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test.1", "@inline @wast(\"test\") function a() {}");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_parse_function_with_native_annotations() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test.1",
            r#"
        // @ts-nocheck

        /**
         * Logs a single i32 value to stdout
         * @param value value to log
         */
        @native("env")
        function println(value: i32) { }
        
        /**
         * Abort the running program
         * @param code error code to abort with
         */
        @native("env")
        function exit(code: i32) { }
        "#,
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }
}
