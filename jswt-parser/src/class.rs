use jswt_ast::*;
use jswt_common::Span;
use jswt_tokenizer::TokenType;

use crate::{consume, ident};
use crate::{ParseError, ParseResult, Parser};

impl<'a> Parser<'a> {
    /// ClassDeclaration
    ///   : 'class' Indentifier ClassBody
    ///   ;
    pub(crate) fn class_declaration(&mut self) -> ParseResult<ClassDeclarationElement> {
        let start = consume!(self, TokenType::Class)?;
        let ident = ident!(self)?;

        let body = self.class_body()?;

        Ok(ClassDeclarationElement {
            span: start + body,
            ident,
        })
    }

    /// ClassBody
    ///   : '{' TODO '}'
    ///   ;
    pub(crate) fn class_body(&mut self) -> ParseResult<Span> {
        let start = consume!(self, TokenType::LeftBrace)?;
        let stop = consume!(self, TokenType::RightBrace)?;

        Ok(start + stop)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use jswt_assert::assert_debug_snapshot;
    use jswt_tokenizer::Tokenizer;

    #[test]
    fn test_class_declaration() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test.1", "class A { }");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }
}
