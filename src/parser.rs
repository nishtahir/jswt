use crate::errors::ParseError;
use crate::expression::{Expr, LiteralExpr};
use crate::node::Node;
use crate::statement::Statement;
use crate::token::{Token, TokenType};

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> Parser {
        Self { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Statement<'a>>, ParseError> {
        let mut statements = Vec::new();
        while !self.is_at_end() {
            let statement = self.statement().unwrap();
            statements.push(statement)
        }
        Ok(statements)
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
        self.consume(TokenType::Print, "Expected print").unwrap();
        self.consume(TokenType::LeftParen, "Expected (").unwrap();
        let expr = self.expression()?;
        self.consume(TokenType::RightParen, "Expected )").unwrap();

        Ok(Statement::print(expr))
    }

    /// let ident = expression
    pub fn variable_statement(&mut self) -> Result<Statement<'a>, ParseError> {
        self.consume(TokenType::Let, "Expected let").unwrap();

        // trying to return a ref to the token here creates a
        // cannot borrow `*self` as mutable more than once at a time
        // since we need to borrow self to parse the expression
        let ident_idx = &self
            .consume(TokenType::Identifier, "Expected 'IDENTIFIER'")
            .unwrap();

        self.consume(TokenType::Equal, "Expected '='").unwrap();
        let expr = self.expression()?;

        // Expected let
        let ident = &self.tokens[*ident_idx];
        Ok(Statement::variable(ident.lexme, expr))
    }

    pub fn function(&mut self) -> Result<Statement<'a>, ParseError> {
        self.consume(TokenType::Function, "Expected 'function'")
            .unwrap();
        self.consume(TokenType::Identifier, "Expected 'IDENTIFIER'")
            .unwrap();
        let ident = self.previous();
        self.consume(TokenType::LeftParen, "Expected '('").unwrap();
        self.consume(TokenType::RightParen, "Expected ')'").unwrap();

        let mut return_type = Option::None;
        if self.check(&TokenType::Colon) {
            self.consume(TokenType::Colon, "Expected 'function'")
                .unwrap();
            self.consume(TokenType::Identifier, "Expected 'IDENTIFIER'")
                .unwrap();
            return_type = Some(self.previous());
        }

        let body = self.block().unwrap();
        Ok(Statement::function(ident, body, return_type))
    }

    pub fn block(&mut self) -> Result<Statement<'a>, ParseError> {
        self.consume(TokenType::LeftBrace, "Expected '{'").unwrap();
        let mut statements = Vec::new();
        while !self.check(&TokenType::RightBrace) {
            let statement = self.statement().unwrap();
            statements.push(statement)
        }
        self.consume(TokenType::RightBrace, "Expected '}'").unwrap();
        Ok(Statement::block(statements))
    }

    pub fn expression(&mut self) -> Result<Expr<'a>, ParseError<'a>> {
        self.primary()
    }

    pub fn primary(&mut self) -> Result<Expr<'a>, ParseError<'a>> {
        if self.match_alt(&[
            TokenType::False,
            TokenType::True,
            TokenType::Number,
            TokenType::String,
        ]) {
            let node = self.previous();
            return Ok(LiteralExpr::new(node).into());
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

    pub fn previous(&self) -> Node<'a> {
        (&self.tokens[self.current - 1]).into()
    }

    pub fn previous_idx(&self) -> usize {
        self.current - 1
    }

    pub fn advance(&mut self) {
        if self.tokens.len() >= self.current {
            self.current += 1;
        }
    }

    pub fn consume(&mut self, token: TokenType, message: &'a str) -> Result<usize, ParseError> {
        if self.check(&token) {
            self.advance();
            return Ok(self.previous_idx());
        }

        Err(ParseError::SyntaxError(message))
    }
}

#[cfg(test)]
mod test {

    use crate::{token::TokenType, Tokenizer};

    use super::*;

    #[test]
    fn test_print_statement() {
        let tokens = Tokenizer::new("print(\"test\")").tokenize().unwrap();
        let actual = Parser::new(tokens).parse().unwrap();
        let expected = vec![Statement::print(Expr::literal(Node::new(
            "test",
            7,
            TokenType::String,
        )))];

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_variable_declaration_statement() {
        let tokens = Tokenizer::new("let user = \"test\"").tokenize().unwrap();
        let actual = Parser::new(tokens).parse().unwrap();
        let expected = vec![Statement::variable(
            "user",
            Expr::literal(Node::new("test", 12, TokenType::String)),
        )];

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_block_declaration_statement() {
        let tokens = Tokenizer::new("{ let user = \"test\" }")
            .tokenize()
            .unwrap();
        let actual = Parser::new(tokens).parse().unwrap();
        let expected = vec![Statement::block(vec![Statement::variable(
            "user",
            Expr::literal(Node::new("test", 14, TokenType::String)),
        )])];

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_void_function_declaration_statement() {
        let tokens = Tokenizer::new("function test() { }").tokenize().unwrap();
        let actual = Parser::new(tokens).parse().unwrap();
        let expected = vec![Statement::function(
            Node::new("test", 9, TokenType::Identifier),
            Statement::block(vec![]),
            None,
        )];

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_function_declaration_statement_with_return_value() {
        let tokens = Tokenizer::new("function test(): i32 { }")
            .tokenize()
            .unwrap();
        let actual = Parser::new(tokens).parse().unwrap();
        let expected = vec![Statement::function(
            Node::new("test", 9, TokenType::Identifier),
            Statement::block(vec![]),
            None,
        )];

        assert_eq!(expected, actual);
    }
}
