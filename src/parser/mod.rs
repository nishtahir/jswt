use crate::ast::expression::Expr;
use crate::ast::function::Param;
use crate::ast::ident::Ident;
use crate::ast::node::Node;
use crate::ast::span::Span;
use crate::ast::statement::Statement;
use crate::ast::types::Type;
use crate::ast::Ast;
use crate::errors::ParseError;
use crate::token::{Token, TokenType};

macro_rules! consume {
    // This macro takes an argument of designator `ident` and
    // creates a function named `$func_name`.
    // The `ident` designator is used for variable/function names.
    ($self:ident, $token:expr) => {
        if $self.check(&($token)) {
            $self.advance();
            &$self.tokens[$self.current - 1]
        } else {
            let current = $self.peek().lexme;
            return Err(ParseError::SyntaxError(format!(
                "Expected '{:?}', found '{:?}'",
                $token, current
            )));
        }
    };
}

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> Parser {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Ast<'a>, ParseError> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            let statement = self.statement().unwrap();
            statements.push(statement)
        }
        Ok(Ast { statements })
    }

    pub fn statement(&mut self) -> Result<Statement<'a>, ParseError> {
        if self.check(&TokenType::Let) {
            return self.variable_statement();
        }

        if self.check(&TokenType::Print) {
            return self.print_statement();
        }

        if self.check(&TokenType::Function) {
            return self.function();
        }

        if self.check(&TokenType::LeftBrace) {
            return self.block();
        }

        todo!()
    }

    /// print(expression)
    pub fn print_statement(&mut self) -> Result<Statement<'a>, ParseError> {
        let start = consume!(self, TokenType::Print).offset;
        consume!(self, TokenType::LeftParen);
        let expr = self.expression()?;
        // add token length to offset
        let end = consume!(self, TokenType::RightParen).offset + 1;

        Ok(Statement::print(expr, start, end))
    }

    /// let ident = expression
    pub fn variable_statement(&mut self) -> Result<Statement<'a>, ParseError> {
        let start = consume!(self, TokenType::Let).offset;

        // trying to return a ref to the token here creates a
        // cannot borrow `*self` as mutable more than once at a time
        // since we need to borrow self to parse the expression
        let ident: Ident = consume!(self, TokenType::Identifier).into();

        consume!(self, TokenType::Equal);
        let expr = self.expression()?;

        let end = self.span_from_index(self.previous_idx());
        Ok(Statement::variable(ident, expr, start, end.end))
    }

    pub fn function(&mut self) -> Result<Statement<'a>, ParseError> {
        consume!(self, TokenType::Function);
        // function name
        let ident = consume!(self, TokenType::Identifier).into();

        // Params
        consume!(self, TokenType::LeftParen);
        let parameters = self.parameters().unwrap();
        consume!(self, TokenType::RightParen);

        let return_type = if self.check(&TokenType::Colon) {
            consume!(self, TokenType::Colon);
            consume!(self, TokenType::Identifier).into()
        } else {
            Type::void(self.span_from_index(self.current))
        };

        let body = self.block().unwrap();

        Ok(Statement::function(ident, parameters, return_type, body))
    }

    pub fn parameters(&mut self) -> Result<Vec<Param<'a>>, ParseError> {
        let mut params = vec![];

        if !self.check(&TokenType::RightParen) {
            loop {
                let ident: Ident = consume!(self, TokenType::Identifier).into();
                consume!(self, TokenType::Colon);
                let ty: Type = consume!(self, TokenType::Identifier).into();

                // let binding to the values because
                // move semantics can be silly sometimes
                let start = ident.span.start;
                let end = ty.span.end;
                params.push(Param::new(ident, ty, start, end));
                if !self.check(&TokenType::Comma) {
                    break;
                }
                consume!(self, TokenType::Comma);
            }
        }
        Ok(params)
    }

    pub fn block(&mut self) -> Result<Statement<'a>, ParseError> {
        consume!(self, TokenType::LeftBrace);
        let mut statements = Vec::new();
        while !self.check(&TokenType::RightBrace) {
            let statement = self.statement().unwrap();
            statements.push(statement)
        }
        consume!(self, TokenType::RightBrace);
        Ok(Statement::block(statements))
    }

    pub fn expression(&mut self) -> Result<Expr<'a>, ParseError> {
        self.primary()
    }

    pub fn primary(&mut self) -> Result<Expr<'a>, ParseError> {
        if self.match_alt(&[TokenType::False, TokenType::True]) {
            let node = self.previous();
            return Ok(Expr::boolean(node));
        }

        if self.match_alt(&[TokenType::Number]) {
            let node = self.previous();
            return Ok(Expr::number(node));
        }

        if self.match_alt(&[TokenType::String]) {
            let node = self.previous();
            return Ok(Expr::string(node));
        }

        todo!()
    }

    pub fn match_alt(&mut self, tokens: &[TokenType]) -> bool {
        for token in tokens {
            if self.check(token) {
                let _ = self.advance();
                return true;
            }
        }
        false
    }

    pub fn check(&self, token: &TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        return self.peek().kind == *token;
    }

    pub fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len() - 1
    }

    pub fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    pub fn span_from_index(&self, idx: usize) -> Span {
        let token = &self.tokens[idx];
        Span::new(token.offset, token.offset + token.lexme.len())
    }

    pub fn previous_ident(&self) -> Ident<'a> {
        let token = &self.tokens[self.current - 1];
        Ident::new(token.lexme, token.offset, token.offset + token.lexme.len())
    }

    pub fn previous_type(&self) -> Type<'a> {
        let token = &self.tokens[self.current - 1];
        Type::new(token.lexme, token.offset, token.offset + token.lexme.len())
    }

    pub fn previous(&self) -> Node<'a> {
        let token = &self.tokens[self.current - 1];
        Node::new(token.lexme, token.offset, token.offset + token.lexme.len())
    }

    pub fn previous_idx(&self) -> usize {
        self.current - 1
    }

    pub fn advance(&mut self) {
        if self.tokens.len() >= self.current {
            self.current += 1;
        }
    }
}

impl<'a> From<&Token<'a>> for Ident<'a> {
    fn from(token: &Token<'a>) -> Self {
        Ident::new(token.lexme, token.offset, token.offset + token.lexme.len())
    }
}

impl<'a> From<&Token<'a>> for Type<'a> {
    fn from(token: &Token<'a>) -> Self {
        Type::new(token.lexme, token.offset, token.offset + token.lexme.len())
    }
}

#[cfg(test)]
mod test {

    use crate::Tokenizer;

    use super::*;

    #[test]
    fn test_print_statement() {
        let tokens = Tokenizer::new("print(\"test\")").tokenize().unwrap();

        let actual = Parser::new(tokens).parse().unwrap().statements;
        let expected = vec![Statement::print(
            Expr::string(Node::new("test", 7, 11)),
            0,
            13,
        )];

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_variable_declaration_statement() {
        let tokens = Tokenizer::new("let user = \"test\"").tokenize().unwrap();
        let actual = Parser::new(tokens).parse().unwrap().statements;
        let expected = vec![Statement::variable(
            Ident::new("user", 4, 8),
            Expr::string(Node::new("test", 12, 16)),
            0,
            16,
        )];

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_block_declaration_statement() {
        let tokens = Tokenizer::new("{ let user = \"test\" }")
            .tokenize()
            .unwrap();
        let actual = Parser::new(tokens).parse().unwrap().statements;
        let expected = vec![Statement::block(vec![Statement::variable(
            Ident::new("user", 6, 10),
            Expr::string(Node::new("test", 14, 18)),
            2,
            18,
        )])];

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_void_function_declaration_statement() {
        let tokens = Tokenizer::new("function test() { }").tokenize().unwrap();
        let actual = Parser::new(tokens).parse().unwrap().statements;
        let expected = vec![Statement::function(
            Ident::new("test", 9, 13),
            vec![],
            Type::void(Span::new(16, 17)),
            Statement::block(vec![]),
        )];

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_function_declaration_statement_with_no_params_with_return_value() {
        let tokens = Tokenizer::new("function test(): i32 { }")
            .tokenize()
            .unwrap();
        let actual = Parser::new(tokens).parse().unwrap().statements;
        let expected = vec![Statement::function(
            Ident::new("test", 9, 13),
            vec![],
            Type::new("i32", 17, 20),
            Statement::block(vec![]),
        )];

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_function_declaration_statement_with_params_and_return_value() {
        let tokens = Tokenizer::new("function test(a: i32): i32 { }")
            .tokenize()
            .unwrap();
        let actual = Parser::new(tokens).parse().unwrap().statements;
        let expected = vec![Statement::function(
            Ident::new("test", 9, 13),
            vec![Param::new(
                Ident::new("a", 14, 15),
                Type::new("i32", 17, 20),
                14,
                20,
            )],
            Type::new("i32", 23, 26),
            Statement::block(vec![]),
        )];

        assert_eq!(expected, actual);
    }
}
