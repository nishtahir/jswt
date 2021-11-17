use crate::ast::ident::Ident;
use crate::ast::program::{
    AssignableElement, BlockStatement, EmptyStatement, FormalParameterArg, FormalParameterList,
    FunctionBody, FunctionDeclarationElement, FunctionDecorators, Program, SourceElement,
    SourceElements, StatementElement, StatementList, VariableModifier, VariableStatement,
};
use crate::errors::ParseError;
use crate::token::{Token, TokenType};
use crate::Tokenizer;

/// Returns true if a token matching the given token type
/// was consumed.
macro_rules! maybe_consume {
    ($self:ident, $token:expr) => {{
        if $self.lookahead_type() == Some($token) {
            consume_unchecked!($self);
            true
        } else {
            false
        }
    }};
}

/// Advances the tokenizer to the next token. This assumes
/// that the caller has done their due dilligence of checking that
/// the current lookahead token is what was expected
macro_rules! consume_unchecked {
    ($self:ident) => {
        $self.lookahead = $self.tokenizer.next_token();
    };
}

/// Checks that the current lookahead tokentype is the same type
/// as the given token then advances the tokenizer to the next token
macro_rules! consume {
    ($self:ident, $token:expr) => {{
        let token = $self.lookahead.as_ref().expect("Expected end of input");
        if token.kind != $token {
            panic!("Mismatched token type")
        }
        // Advance lookahead
        $self.lookahead = $self.tokenizer.next_token();
    }};
}

macro_rules! ident {
    // This macro takes an argument of designator `ident` and
    // creates a function named `$func_name`.
    // The `ident` designator is used for variable/function names.
    ($self:ident) => {{
        let token = $self.lookahead.as_ref().expect("Expected end of input");
        if token.kind != TokenType::Identifier {
            panic!("Mismatched token type {:?}", token);
        }
        let ident = Ident::from(token);
        // Advance lookahead
        $self.lookahead = $self.tokenizer.next_token();
        ident
    }};
}

/// Predictive LL(1) parser
pub struct Parser<'a> {
    tokenizer: Tokenizer<'a>,
    lookahead: Option<Token<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(tokenizer: Tokenizer<'a>) -> Parser {
        Self {
            tokenizer,
            lookahead: None,
        }
    }

    pub fn parse(&mut self) -> Result<Program<'a>, ParseError> {
        // Seed the look ahead for the entry point
        self.lookahead = self.tokenizer.next_token();
        self.program()
    }

    /// Entry point of the program
    ///
    /// Program
    ///   :  SourceElements?
    ///   ;
    fn program(&mut self) -> Result<Program<'a>, ParseError> {
        Ok(Program {
            // Read until the end of the file
            source_elements: self.source_elements(None)?,
        })
    }

    /// SourceElements
    ///   :  SourceElement
    ///   |  SourceElements SourceElement
    ///   ;
    fn source_elements(
        &mut self,
        terminal: Option<TokenType>,
    ) -> Result<SourceElements<'a>, ParseError> {
        let mut source_elements = vec![];
        while self.lookahead_type().is_some() && self.lookahead_type() != terminal {
            source_elements.push(self.source_element()?);
        }
        Ok(SourceElements { source_elements })
    }

    /// SourceElement
    ///   :  FunctionDeclaration
    ///   |  Statement
    ///   ;
    fn source_element(&mut self) -> Result<SourceElement<'a>, ParseError> {
        let elem = match self.lookahead_type() {
            // Need to check for optional function decorators
            Some(TokenType::Function) | Some(TokenType::Export) => {
                self.function_declaration()?.into()
            }
            _ => self.statement()?.into(),
        };
        Ok(elem)
    }

    /// Statement
    ///   :  Block
    ///   |  EmptyStatement
    ///   |  VariableStatement
    ///   ;
    fn statement(&mut self) -> Result<StatementElement<'a>, ParseError> {
        let elem = match self.lookahead_type() {
            Some(TokenType::LeftBrace) => self.block()?.into(),
            Some(TokenType::Semi) => self.empty_statement()?.into(),
            Some(TokenType::Let) | Some(TokenType::Const) => self.variable_statement()?.into(),
            _ => todo!(),
        };
        Ok(elem)
    }

    /// Block
    ///   :  '{' statementList? '}'
    ///   ;
    fn block(&mut self) -> Result<BlockStatement<'a>, ParseError> {
        consume!(self, TokenType::LeftBrace);
        let statements = self.statement_list(Some(TokenType::RightBrace))?;
        consume!(self, TokenType::RightBrace);
        Ok(BlockStatement::new(statements))
    }

    /// StatementList
    ///   :  Statement
    ///   |  StatementList Statement
    ///   ;
    fn statement_list(
        &mut self,
        terminal: Option<TokenType>,
    ) -> Result<StatementList<'a>, ParseError> {
        let mut statements = vec![];
        while self.lookahead_type().is_some() && self.lookahead_type() != terminal {
            statements.push(self.statement()?);
        }
        Ok(StatementList::new(statements))
    }

    /// EmptyStatement
    ///   : ';'
    ///   ;
    fn empty_statement(&mut self) -> Result<EmptyStatement, ParseError> {
        consume!(self, TokenType::Semi);
        Ok(EmptyStatement::default())
    }

    /// VariableStatement
    ///   :  VariableModifier Assignable ('=' singleExpression)?
    fn variable_statement(&mut self) -> Result<VariableStatement<'a>, ParseError> {
        let modifier = self.variable_modifier()?;
        let assignable = self.assignable()?;
        consume!(self, TokenType::Equal);

        Ok(VariableStatement::new(modifier, assignable))
    }

    /// VariableModifier
    ///   : 'let'
    ///   | 'const'
    ///   ;
    fn variable_modifier(&mut self) -> Result<VariableModifier, ParseError> {
        let modifier = match self.lookahead_type() {
            Some(TokenType::Let) => VariableModifier::Let,
            Some(TokenType::Const) => VariableModifier::Const,
            // it should never be anything but these
            _ => unreachable!(),
        };
        // Eat the modifier token
        consume_unchecked!(self);
        Ok(modifier)
    }

    /// Assignable
    ///   : Ident
    ///   ;
    fn assignable(&mut self) -> Result<AssignableElement<'a>, ParseError> {
        Ok(AssignableElement::Identifier(ident!(self)))
    }

    /// FunctionDeclaration
    ///   :  'export'? 'function' Identifier ( FormalParameterList? ) TypeAnnotation? FunctionBody
    ///   ;
    fn function_declaration(&mut self) -> Result<FunctionDeclarationElement<'a>, ParseError> {
        let has_export = maybe_consume!(self, TokenType::Export);
        consume!(self, TokenType::Function);

        let ident = ident!(self);

        consume!(self, TokenType::LeftParen);
        let params = self.formal_parameter_list()?;
        consume!(self, TokenType::RightParen);

        //Parse return value
        let mut returns = None;
        if self.lookahead_type() == Some(TokenType::Colon) {
            returns = Some(self.type_annotation()?);
        }

        let body = self.function_body()?;

        let decorators = FunctionDecorators { export: has_export };
        Ok(FunctionDeclarationElement::new(
            decorators, ident, params, returns, body,
        ))
    }

    /// FormalParameterList
    ///   :  FormalParameterArg
    ///   |  FormalParameterArg , FormalParameterArg
    ///   ;
    fn formal_parameter_list(&mut self) -> Result<FormalParameterList<'a>, ParseError> {
        let mut params = vec![];
        if self.lookahead_type() != Some(TokenType::RightParen) {
            loop {
                params.push(self.formal_parameter_arg()?);
                if self.lookahead_type() != Some(TokenType::Comma) {
                    break;
                }
                consume!(self, TokenType::Comma)
            }
        }
        Ok(FormalParameterList::new(params))
    }

    /// FormalParameterArg
    ///   :  Ident TypeAnnotation
    ///   ;
    fn formal_parameter_arg(&mut self) -> Result<FormalParameterArg<'a>, ParseError> {
        let ident = ident!(self);
        let type_param = self.type_annotation()?;
        Ok(FormalParameterArg::new(ident, type_param))
    }

    /// TypeAnnotation
    ///   : ':' Ident
    ///   ;
    fn type_annotation(&mut self) -> Result<Ident<'a>, ParseError> {
        consume!(self, TokenType::Colon);
        let type_param = ident!(self);
        Ok(type_param)
    }

    ///  FunctionBody
    ///    :  '{' SourceElements? '}'
    ///    ;
    fn function_body(&mut self) -> Result<FunctionBody<'a>, ParseError> {
        consume!(self, TokenType::LeftBrace);
        // Read until we find a closing brace
        let source_elements = self.source_elements(Some(TokenType::RightBrace))?;
        consume!(self, TokenType::RightBrace);
        Ok(FunctionBody::new(source_elements))
    }

    /// Return an owned token type value of the
    /// current lookahead token
    fn lookahead_type(&self) -> Option<TokenType> {
        let lookahed = &self.lookahead;
        lookahed.as_ref().map(|token| token.kind)
    }
}

impl<'a> From<&Token<'a>> for Ident<'a> {
    fn from(token: &Token<'a>) -> Self {
        Ident::new(token.lexme, token.offset, token.offset + token.lexme.len())
    }
}

#[cfg(test)]
mod test {
    use std::vec;

    use super::*;
    use crate::{
        ast::{program::FunctionDeclarationElement, span::Span},
        Tokenizer,
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn test_function_declaration_statement() {
        let tokenizer = Tokenizer::new("function test() { }");
        let actual = Parser::new(tokenizer).parse().unwrap();
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![SourceElement::FunctionDeclaration(
                    FunctionDeclarationElement {
                        decorators: FunctionDecorators { export: false },
                        ident: Ident {
                            span: Span { start: 9, end: 13 },
                            value: "test",
                        },
                        params: FormalParameterList { parameters: vec![] },
                        returns: None,
                        body: FunctionBody::new(SourceElements {
                            source_elements: vec![],
                        }),
                    },
                )],
            },
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_empty_program() {
        let tokenizer = Tokenizer::new("");
        let actual = Parser::new(tokenizer).parse().unwrap();
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![],
            },
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_function_declaration_statement_with_one_param() {
        let tokenizer = Tokenizer::new("function name(a: i32) { }");
        let actual = Parser::new(tokenizer).parse().unwrap();
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![SourceElement::FunctionDeclaration(
                    FunctionDeclarationElement {
                        decorators: FunctionDecorators { export: false },
                        ident: Ident {
                            span: Span { start: 9, end: 13 },
                            value: "name",
                        },
                        params: FormalParameterList {
                            parameters: vec![FormalParameterArg {
                                ident: Ident {
                                    span: Span { start: 14, end: 15 },
                                    value: "a",
                                },
                                type_annotation: Ident {
                                    span: Span { start: 17, end: 20 },
                                    value: "i32",
                                },
                            }],
                        },
                        returns: None,
                        body: FunctionBody::new(SourceElements {
                            source_elements: vec![],
                        }),
                    },
                )],
            },
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_function_declaration_statement_with_two_params() {
        let tokenizer = Tokenizer::new("function name(a: i32, b: f32) { }");
        let actual = Parser::new(tokenizer).parse().unwrap();
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![SourceElement::FunctionDeclaration(
                    FunctionDeclarationElement {
                        decorators: FunctionDecorators { export: false },
                        ident: Ident {
                            span: Span { start: 9, end: 13 },
                            value: "name",
                        },
                        params: FormalParameterList {
                            parameters: vec![
                                FormalParameterArg {
                                    ident: Ident {
                                        span: Span { start: 14, end: 15 },
                                        value: "a",
                                    },
                                    type_annotation: Ident {
                                        span: Span { start: 17, end: 20 },
                                        value: "i32",
                                    },
                                },
                                FormalParameterArg {
                                    ident: Ident {
                                        span: Span { start: 22, end: 23 },
                                        value: "b",
                                    },
                                    type_annotation: Ident {
                                        span: Span { start: 25, end: 28 },
                                        value: "f32",
                                    },
                                },
                            ],
                        },
                        returns: None,
                        body: FunctionBody::new(SourceElements {
                            source_elements: vec![],
                        }),
                    },
                )],
            },
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_function_declaration_statement_with_export_decorator() {
        let tokenizer = Tokenizer::new("export function test() { }");
        let actual = Parser::new(tokenizer).parse().unwrap();
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![SourceElement::FunctionDeclaration(
                    FunctionDeclarationElement {
                        decorators: FunctionDecorators { export: true },
                        ident: Ident {
                            span: Span { start: 16, end: 20 },
                            value: "test",
                        },
                        params: FormalParameterList { parameters: vec![] },
                        returns: None,
                        body: FunctionBody::new(SourceElements {
                            source_elements: vec![],
                        }),
                    },
                )],
            },
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_function_declaration_statement_with_return_value() {
        let tokenizer = Tokenizer::new("function test(): i32 { }");
        let actual = Parser::new(tokenizer).parse().unwrap();
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![SourceElement::FunctionDeclaration(
                    FunctionDeclarationElement {
                        decorators: FunctionDecorators { export: false },
                        ident: Ident {
                            span: Span { start: 9, end: 13 },
                            value: "test",
                        },
                        params: FormalParameterList { parameters: vec![] },
                        returns: Some(Ident {
                            span: Span { start: 17, end: 20 },
                            value: "i32",
                        }),
                        body: FunctionBody::new(SourceElements {
                            source_elements: vec![],
                        }),
                    },
                )],
            },
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_function_declaration_statement_with_two_params_and_return_value() {
        let tokenizer = Tokenizer::new("function test(a: i32, b: i32): i32 { }");
        let actual = Parser::new(tokenizer).parse().unwrap();
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![SourceElement::FunctionDeclaration(
                    FunctionDeclarationElement {
                        decorators: FunctionDecorators { export: false },
                        ident: Ident {
                            span: Span { start: 9, end: 13 },
                            value: "test",
                        },
                        params: FormalParameterList {
                            parameters: vec![
                                FormalParameterArg {
                                    ident: Ident {
                                        span: Span { start: 14, end: 15 },
                                        value: "a",
                                    },
                                    type_annotation: Ident {
                                        span: Span { start: 17, end: 20 },
                                        value: "i32",
                                    },
                                },
                                FormalParameterArg {
                                    ident: Ident {
                                        span: Span { start: 22, end: 23 },
                                        value: "b",
                                    },
                                    type_annotation: Ident {
                                        span: Span { start: 25, end: 28 },
                                        value: "i32",
                                    },
                                },
                            ],
                        },
                        returns: Some(Ident {
                            span: Span { start: 31, end: 34 },
                            value: "i32",
                        }),
                        body: FunctionBody::new(SourceElements {
                            source_elements: vec![],
                        }),
                    },
                )],
            },
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_empty_block() {
        let tokenizer = Tokenizer::new("{}");
        let actual = Parser::new(tokenizer).parse().unwrap();
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![SourceElement::Statement(StatementElement::Block(
                    BlockStatement {
                        statements: StatementList { statements: vec![] },
                    },
                ))],
            },
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_nested_empty_blocks() {
        let tokenizer = Tokenizer::new("{ {} }");
        let actual = Parser::new(tokenizer).parse().unwrap();
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![SourceElement::Statement(StatementElement::Block(
                    BlockStatement {
                        statements: StatementList {
                            statements: vec![StatementElement::Block(BlockStatement {
                                statements: StatementList { statements: vec![] },
                            })],
                        },
                    },
                ))],
            },
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_function_declaration_statement_with_block_body() {
        let tokenizer = Tokenizer::new("function test() { {} }");
        let actual = Parser::new(tokenizer).parse().unwrap();
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![SourceElement::FunctionDeclaration(
                    FunctionDeclarationElement {
                        decorators: FunctionDecorators { export: false },
                        ident: Ident {
                            span: Span { start: 9, end: 13 },
                            value: "test",
                        },
                        params: FormalParameterList { parameters: vec![] },
                        returns: None,
                        body: FunctionBody::new(SourceElements {
                            source_elements: vec![SourceElement::Statement(
                                StatementElement::Block(BlockStatement {
                                    statements: StatementList { statements: vec![] },
                                }),
                            )],
                        }),
                    },
                )],
            },
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_empty_statement() {
        let tokenizer = Tokenizer::new(";");
        let actual = Parser::new(tokenizer).parse().unwrap();
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![SourceElement::Statement(StatementElement::Empty(
                    EmptyStatement {},
                ))],
            },
        };
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_parse_variable_statement() {
        let tokenizer = Tokenizer::new("let x = ");
        let actual = Parser::new(tokenizer).parse().unwrap();
        let expected = Program {
            source_elements: SourceElements {
                source_elements: vec![SourceElement::Statement(StatementElement::Variable(
                    VariableStatement {
                        modifier: VariableModifier::Let,
                        target: AssignableElement::Identifier(Ident {
                            span: Span { start: 4, end: 5 },
                            value: "x",
                        }),
                    },
                ))],
            },
        };
        assert_eq!(expected, actual);
    }
}
