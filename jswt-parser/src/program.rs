use jswt_ast::{File, Program, SourceElement, SourceElements};
use jswt_tokenizer::TokenType;

use crate::{consume_unchecked, maybe_consume, ParseError, ParseResult, Parser};

impl<'a> Parser<'a> {
    ////
    //// Program
    ////   :  File*
    ////   ;
    ////
    pub(crate) fn program(&mut self) -> Program {
        let mut files = vec![];
        while self.lookahead.is_some() {
            files.push(self.file());
        }
        Program { files }
    }

    ////
    //// File
    ////   : SourceElements? Eof
    ////   ;
    ////
    pub(crate) fn file(&mut self) -> File {
        let start = self.lookahead_span();
        let source_elements = self.source_elements(Some(TokenType::Eof));
        // Eat the EOF token
        let end = consume_unchecked!(self);
        File {
            span: start + end,
            source_elements,
        }
    }

    ////
    //// SourceElements
    ////   :  SourceElement
    ////   |  SourceElements SourceElement
    ////   ;
    ////
    pub(crate) fn source_elements(&mut self, terminal: Option<TokenType>) -> SourceElements {
        let mut source_elements = vec![];
        let start = self.lookahead_span();
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
                        TokenType::Return,
                        TokenType::Let,
                        TokenType::Const,
                        TokenType::Eof,
                    ],
                ),
            };
        }

        let end = self.lookahead_span();
        SourceElements {
            span: start + end,
            source_elements,
        }
    }

    ////
    //// SourceElement
    ////  :  Annotation* export? (FunctionDeclaration |  ClassDeclaration |  VariableDeclaration)
    ////  ;
    ////
    fn source_element(&mut self) -> ParseResult<SourceElement> {
        // Check for annotations
        let mut annotations = vec![];
        while self.lookahead_is(TokenType::At) {
            annotations.push(self.annotation()?);
        }

        // Check for export
        let export = maybe_consume!(self, TokenType::Export);

        let elem = match self.lookahead_type() {
            // Need to check for optional function decorators
            Some(TokenType::Function) => self
                .function_declaration(annotations, export.is_some())?
                .into(),
            Some(TokenType::Class) => self
                .class_declaration(annotations, export.is_some())?
                .into(),
            Some(TokenType::Let) | Some(TokenType::Const) => self
                .variable_declaration(annotations, export.is_some())?
                .into(),
            Some(TokenType::Import) => self.import_declaration()?.into(),
            _ => {
                return Err(ParseError::NoViableAlternative {
                    expected: vec![
                        TokenType::Function,
                        TokenType::Class,
                        TokenType::Let,
                        TokenType::Const,
                    ],
                    actual: self.lookahead_type().unwrap(),
                    span: self.lookahead_span(),
                })
            }
        };
        Ok(elem)
    }
}
