use crate::{consume, ident, ParseError, ParseResult, Parser};

use jswt_ast::*;
use jswt_common::{Span, Spannable};
use jswt_tokenizer::TokenType;

impl<'a> Parser<'a> {
    /// ClassDeclaration
    ///   : 'class' Indentifier ClassBody
    ///   ;
    pub(crate) fn class_declaration(&mut self) -> ParseResult<ClassDeclarationElement> {
        let start = consume!(self, TokenType::Class)?;
        let ident = ident!(self)?;

        let body = self.class_body()?;

        Ok(ClassDeclarationElement {
            span: start + body.span(),
            ident,
            body,
        })
    }

    /// ClassBody
    ///   : '{' ClassElement* '}'
    ///   ;
    pub(crate) fn class_body(&mut self) -> ParseResult<ClassBody> {
        let start = consume!(self, TokenType::LeftBrace)?;

        let mut class_elements = vec![];
        while !self.lookahead_is(TokenType::RightBrace) {
            class_elements.push(self.class_element()?);
        }

        let end = consume!(self, TokenType::RightBrace)?;
        Ok(ClassBody {
            span: start + end,
            class_elements,
        })
    }

    /// ClassElement
    ///   : ClassConstructor
    ///   ;
    pub(crate) fn class_element(&mut self) -> ParseResult<ClassElement> {
        let elem = match self.lookahead_type() {
            Some(TokenType::Constructor) => self.class_constructor()?.into(),
            Some(TokenType::Identifier) => self.class_property_member()?.into(),
            _ => todo!(),
        };

        Ok(elem)
    }

    /// ClassConstructor
    ///   : 'constructor' '(' FormalParameterList ')' Block
    ///   ;
    pub(crate) fn class_constructor(&mut self) -> ParseResult<ClassConstructorElement> {
        let start = consume!(self, TokenType::Constructor)?;
        consume!(self, TokenType::LeftParen)?;
        let params = self.formal_parameter_list()?;
        consume!(self, TokenType::RightParen)?;

        let body = self.block()?;

        Ok(ClassConstructorElement {
            span: start + body.span(),
            params,
            body,
        })
    }

    /// ClassPropertyMember
    ///   : Annotation? Identifier '(' FormalParameterList ')' ':' TypeAnnotation Block    #ClassMethod
    ///   : Annotation? Identifier ':' TypeAnnotaiton     #ClassField
    ///   ;
    pub(crate) fn class_property_member(&mut self) -> ParseResult<ClassElement> {
        let mut annotations = vec![];
        while self.lookahead_is(TokenType::At) {
            annotations.push(self.annotation()?);
        }

        let ident = ident!(self)?;
        if self.lookahead_is(TokenType::LeftParen) {
            consume!(self, TokenType::LeftParen)?;
            let params = self.formal_parameter_list()?;
            consume!(self, TokenType::RightParen)?;

            //Parse return value
            let mut returns = None;
            if self.lookahead_is(TokenType::Colon) {
                returns = Some(self.type_annotation()?);
            }

            let body = self.block()?;

            return Ok(ClassElement::Method(ClassMethodElement {
                span: ident.span() + body.span(),
                ident,
                params,
                returns,
                body,
                annotations,
            }));
        }

        let type_annotation = self.type_annotation()?;
        consume!(self, TokenType::Semi)?;

        Ok(ClassElement::Field(ClassFieldElement {
            span: ident.span(),
            annotations,
            ident,
            type_annotation,
        }))
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
        tokenizer.enqueue_source_str("test.1", "class A { constructor(a: i32) { } }");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_class_method_declaration() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test.1", "class A { hello(a: i32) { } }");
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }

    #[test]
    fn test_class_method_declaration_with_this_binding() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test.1",
            r"
            class A { 
                constructor(a: i32) { 
                    this.a = a;
                }
            }
        ",
        );
        let mut parser = Parser::new(&mut tokenizer);
        let actual = parser.parse();
        assert_debug_snapshot!(actual);
        assert_eq!(parser.errors.len(), 0);
    }
}
