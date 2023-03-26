use jswt_ast::*;
use jswt_common::{Span, Spannable};
use jswt_tokenizer::TokenType;

use crate::{consume, ident};
use crate::{ParseError, ParseResult, Parser};

impl<'a> Parser<'a> {
    //// FunctionDeclaration
    ////   :  'function' Identifier ( FormalParameterList? ) TypeAnnotation? FunctionBody
    ////   ;
    pub(crate) fn function_declaration(
        &mut self,
        // How do I pass the start span of the annotations/export keyword?
        annotations: Vec<Annotation>,
        export: bool,
    ) -> ParseResult<FunctionDeclarationElement> {
        let function_span = consume!(self, TokenType::Function)?;
        let ident = ident!(self)?;
        let params = self.formal_parameter_list()?;

        //Parse return value
        let mut returns = None;
        if self.lookahead_is(TokenType::Colon) {
            returns = Some(self.type_annotation()?);
        }

        let body = self.block_statement()?;

        let decorators = FunctionDecorators {
            annotations,
            export,
        };
        Ok(FunctionDeclarationElement {
            span: function_span + body.span(),
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
        tokenizer.enqueue_source_str("test_function_declaration_statement", "function test() { }");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_function_declaration_statement_with_one_param() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_function_declaration_statement_with_one_param",
            "function name(a: i32) { }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_function_declaration_statement_with_two_params() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_function_declaration_statement_with_two_params",
            "function name(a: i32, b: f32) { }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_function_declaration_statement_with_export_decorator() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_function_declaration_statement_with_export_decorator",
            "export function test() { }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_function_declaration_statement_with_return_value() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_function_declaration_statement_with_return_value",
            "function test(): i32 { }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_parse_function_declaration_statement_with_two_params_and_return_value() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_function_declaration_statement_with_two_params_and_return_value",
            "function test(a: i32, b: i32): i32 { }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_function_declaration_statement_with_block_body() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_function_declaration_statement_with_block_body",
            "function test() { {} }",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_parse_function_with_multiple_annotations() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_function_with_multiple_annotations",
            "@inline @wast(\"test\") function a() {}",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_eq!(parser.errors.len(), 0);
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_parse_function_with_native_annotations() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_parse_function_with_native_annotations",
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
