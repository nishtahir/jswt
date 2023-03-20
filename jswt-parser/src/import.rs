use jswt_ast::{Annotation, ImportDeclarationElement};
use jswt_common::{Span, Spannable};
use jswt_tokenizer::TokenType;

use crate::{consume, ident, Identifier, ParseError, ParseResult, Parser};

impl<'a> Parser<'a> {
    ///VariableDeclaration
    ///  :  VariableModifier? Identifier TypeAnnotation? '=' SingleExpression ';'
    ///  ;
    pub(crate) fn import_declaration(&mut self) -> ParseResult<ImportDeclarationElement> {
        // Consume the import token
        let modifier = consume!(self, TokenType::Import)?;
        // Consume the import path string
        let _ = consume!(self, TokenType::String)?;
        // Consume the semicolon
        let end = consume!(self, TokenType::Semi)?;
        Ok(ImportDeclarationElement {
            span: modifier + end,
        })
    }
}

#[cfg(test)]
mod test {
    use std::fs;

    use super::*;
    use jswt_assert::assert_debug_snapshot;
    use jswt_tokenizer::Tokenizer;

    #[test]
    fn test_parse_import_declaration() {
        let mut tokenizer = Tokenizer::default();
        fs::write("./target/test.jswt", "const x = 0;").unwrap();
        fs::write("./target/test_parse_import_declaration.jswt", "import \"./test.jswt\";").unwrap();
        tokenizer.enqueue_source("./target/test_parse_import_declaration.jswt");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
    }
}
