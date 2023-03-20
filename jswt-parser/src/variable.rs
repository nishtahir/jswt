use jswt_ast::{Annotation, VariableDeclarationElement};
use jswt_common::{Span, Spannable};
use jswt_tokenizer::TokenType;

use crate::{consume, ident, Identifier, ParseError, ParseResult, Parser};

impl<'a> Parser<'a> {
    ///VariableDeclaration
    ///  :  VariableModifier? Identifier TypeAnnotation? '=' SingleExpression ';'
    ///  ;
    pub(crate) fn variable_declaration(
        &mut self,
        annotations: Vec<Annotation>,
        export: bool,
    ) -> ParseResult<VariableDeclarationElement> {
        let modifier = self.variable_modifier()?;
        let name = ident!(self)?;

        let mut type_annotation = None;
        if self.lookahead_is(TokenType::Colon) {
            type_annotation = Some(self.type_annotation()?);
        }

        consume!(self, TokenType::Equal)?;

        let expression = self.single_expression()?;
        let end = consume!(self, TokenType::Semi)?;

        Ok(VariableDeclarationElement {
            annotations,
            export,
            span: modifier.span() + end,
            modifier,
            name,
            expression,
            type_annotation,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use jswt_assert::assert_debug_snapshot;
    use jswt_tokenizer::Tokenizer;

    #[test]
    fn test_basic_variable_declaration() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_basic_variable_declaration", "const x = 1;");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_variable_declaration_with_type_annotation() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_variable_declaration_with_type_annotation",
            "@Test const x: i32 = 1;",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_variable_declaration_with_export() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_variable_declaration_with_export", "export const x = 1;");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
    }
}
