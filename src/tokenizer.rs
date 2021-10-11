use crate::token::{Token, TokenType};
use crate::{errors::TokenizerError, iter::LineColumnIterator};
use std::str::Chars;

pub struct Tokenizer<'a> {
    source: &'a str,
    iter: LineColumnIterator<Chars<'a>>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Tokenizer<'a> {
        Tokenizer {
            source: input,
            iter: LineColumnIterator::new(input.chars()),
        }
    }

    /// Tokenize the source input string
    pub fn tokenize(&mut self) -> Result<Vec<Token<'a>>, TokenizerError> {
        let mut tokens = Vec::new();
        while let Some(c) = self.iter.peek() {
            match c {
                '{' => {
                    let token = self.read_token(TokenType::LeftBrace)?;
                    tokens.push(token);
                }
                '}' => {
                    let token = self.read_token(TokenType::RightBrace)?;
                    tokens.push(token);
                }
                '(' => {
                    let token = self.read_token(TokenType::LeftParen)?;
                    tokens.push(token);
                }
                ')' => {
                    let token = self.read_token(TokenType::RightParen)?;
                    tokens.push(token);
                }
                ',' => {
                    let token = self.read_token(TokenType::Comma)?;
                    tokens.push(token);
                }
                ';' => {
                    let token = self.read_token(TokenType::Semi)?;
                    tokens.push(token);
                }
                '<' | '>' | '/' => {
                    let token = self.read_operator()?;
                    tokens.push(token);
                }
                '"' => {
                    let token = self.read_string()?;
                    tokens.push(token);
                }
                '0'..='9' => {
                    let token = self.read_number()?;
                    tokens.push(token);
                }
                'a'..='z' | 'A'..='Z' => {
                    let token = self.read_identifier()?;
                    tokens.push(token);
                }
                '\n' | '\t' | ' ' => {
                    // skip
                    self.iter.next();
                }
                _ => return Err(TokenizerError::UnexpectedEof),
            }
        }

        // Add EOF marker
        tokens.push(Token::eof(self.iter.offset()));
        Ok(tokens)
    }

    pub fn has_more_tokens(&self) -> bool {
        self.iter.offset() <= self.source.len()
    }

    /// Consume the next token from the iterator
    fn read_token(&mut self, kind: TokenType) -> Result<Token<'a>, TokenizerError> {
        let offset = self.iter.offset();
        match self.iter.next() {
            Some(_) => {
                let lexme = &self.source[offset..offset + 1];
                let token = Token::new(lexme, kind, offset);
                Ok(token)
            }

            None => Err(TokenizerError::UnexpectedEof),
        }
    }

    fn read_number(&mut self) -> Result<Token<'a>, TokenizerError> {
        let start = self.iter.offset();
        let mut end = start;

        while let Some(&ch) = self.iter.peek() {
            if !ch.is_digit(10) && ch != '.' {
                break;
            }
            end += 1;
            self.iter.next();
        }

        let value = &self.source[start..end];
        Ok(Token::new(value, TokenType::Number, start))
    }

    fn read_identifier(&mut self) -> Result<Token<'a>, TokenizerError> {
        let start = self.iter.offset();
        let mut end = start;

        while let Some(&ch) = self.iter.peek() {
            if !ch.is_alphabetic() && ch != '.' {
                break;
            }
            end += 1;
            self.iter.next();
        }

        let value = &self.source[start..end];
        let token_type = match value {
            "function" => TokenType::Function,
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "return" => TokenType::Return,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "let" => TokenType::Let,
            "print" => TokenType::Print,
            _ => TokenType::Identifier,
        };

        Ok(Token::new(value, token_type, start))
    }

    fn read_string(&mut self) -> Result<Token<'a>, TokenizerError> {
        // Consume opening double quote
        self.iter.next();

        let start = self.iter.offset();
        let mut end = start;
        while let Some(&ch) = self.iter.peek() {
            if ch == '"' {
                break;
            }
            end += 1;
            self.iter.next();
        }

        // Consume closing double quote
        self.iter.next();

        let lexme = &self.source[start..end];
        Ok(Token::new(lexme, TokenType::String, start))
    }

    fn read_operator(&mut self) -> Result<Token<'a>, TokenizerError> {
        let start = self.iter.offset();

        // Consume known first char
        let first = self.iter.next().ok_or(TokenizerError::UnexpectedEof)?;

        let second = self.iter.peek();

        match first {
            '/' => {
                if second == Some(&'/') {
                    self.consume_while(|a| a != '\n');
                    let end = self.iter.offset();
                    let lexme = &self.source[start..end];
                    Ok(Token::new(lexme, TokenType::Comment, start))
                } else {
                    self.iter.next();
                    let end = self.iter.offset();
                    let lexme = &self.source[start..end];
                    Ok(Token::new(lexme, TokenType::Div, start))
                }
            }
            '<' => {
                if second == Some(&'=') {
                    // Advance iterator
                    self.iter.next();

                    let end = self.iter.offset();

                    let lexme = &self.source[start..end];
                    Ok(Token::new(lexme, TokenType::LessEqual, start))
                } else {
                    let lexme = &self.source[start..];
                    Ok(Token::new(lexme, TokenType::Less, start))
                }
            }
            '>' => {
                if second == Some(&'=') {
                    // Advance iterator
                    self.iter.next();

                    let end = self.iter.offset();

                    let lexme = &self.source[start..end];
                    Ok(Token::new(lexme, TokenType::GreaterEqual, start))
                } else {
                    let lexme = &self.source[start..];
                    Ok(Token::new(lexme, TokenType::Greater, start))
                }
            }
            _ => todo!(),
        }
    }

    fn consume_while<F>(&mut self, predicate: F)
    where
        F: Fn(char) -> bool,
    {
        while let Some(&ch) = self.iter.peek() {
            if predicate(ch) {
                self.iter.next();
            } else {
                break;
            }
        }
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn test_tokenize_number() {
        let mut tokenizer = Tokenizer::new("42");
        let actual = tokenizer.tokenize().unwrap();
        let expected = vec![Token::new("42", TokenType::Number, 0), Token::eof(2)];
        assert_eq!(expected, actual)
    }

    #[test]
    fn test_tokenize_string() {
        let mut tokenizer = Tokenizer::new("\"Hello World\"");
        let actual = tokenizer.tokenize().unwrap();
        let expected = vec![Token::new("Hello World", TokenType::String, 1)];
        assert_eq!(expected, actual)
    }

    #[test]
    fn test_tokenize_braces() {
        let mut tokenizer = Tokenizer::new("{}");
        let actual = tokenizer.tokenize().unwrap();
        let expected = vec![
            Token::new("{", TokenType::LeftBrace, 0),
            Token::new("}", TokenType::RightBrace, 1),
            Token::eof(2),
        ];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenize_parens() {
        let mut tokenizer = Tokenizer::new("()");
        let actual = tokenizer.tokenize().unwrap();
        let expected = vec![
            Token::new("(", TokenType::LeftParen, 0),
            Token::new(")", TokenType::RightParen, 1),
            Token::eof(2),
        ];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenize_comma() {
        let mut tokenizer = Tokenizer::new(",");
        let actual = tokenizer.tokenize().unwrap();
        let expected = vec![Token::new(",", TokenType::Comma, 0), Token::eof(1)];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenize_less_than() {
        let mut tokenizer = Tokenizer::new("<");
        let actual = tokenizer.tokenize().unwrap();
        let expected = vec![Token::new("<", TokenType::Less, 0), Token::eof(1)];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenize_less_than_equal() {
        let mut tokenizer = Tokenizer::new("<=");
        let actual = tokenizer.tokenize().unwrap();
        let expected = vec![Token::new("<=", TokenType::LessEqual, 0), Token::eof(2)];
        assert_eq!(expected, actual);
    }
    #[test]
    fn test_tokenize_greater_than() {
        let mut tokenizer = Tokenizer::new(">");
        let actual = tokenizer.tokenize().unwrap();
        let expected = vec![Token::new(">", TokenType::Greater, 0), Token::eof(2)];
        assert_eq!(expected, actual);
    }
    #[test]
    fn test_tokenize_greater_than_equal() {
        let mut tokenizer = Tokenizer::new(">=");
        let actual = tokenizer.tokenize().unwrap();
        let expected = vec![Token::new(">=", TokenType::GreaterEqual, 0), Token::eof(2)];
        assert_eq!(expected, actual);
    }
    #[test]
    fn test_tokenize_semi_colon() {
        let mut tokenizer = Tokenizer::new(";");
        let actual = tokenizer.tokenize().unwrap();
        let expected = vec![Token::new(";", TokenType::Semi, 0), Token::eof(1)];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenize_comment() {
        let mut tokenizer = Tokenizer::new("// This is a test comment");
        let actual = tokenizer.tokenize().unwrap();
        let expected = vec![
            Token::new("// This is a test comment", TokenType::Comment, 0),
            Token::eof(30),
        ];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_whitespace_generates_no_tokens() {
        let mut tokenizer = Tokenizer::new("  ");
        let actual = tokenizer.tokenize().unwrap();
        let expected: Vec<Token> = vec![Token::eof(2)];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenize_identifiers() {
        let mut tokenizer = Tokenizer::new("user identifier");
        let actual = tokenizer.tokenize().unwrap();
        let expected: Vec<Token> = vec![
            Token::new("user", TokenType::Identifier, 0),
            Token::new("identifier", TokenType::Identifier, 5),
            Token::eof(30),
        ];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenize_keywords() {
        let mut tokenizer = Tokenizer::new("let false true return function");
        let actual = tokenizer.tokenize().unwrap();
        let expected: Vec<Token> = vec![
            Token::new("let", TokenType::Let, 0),
            Token::new("false", TokenType::False, 4),
            Token::new("true", TokenType::True, 10),
            Token::new("return", TokenType::Return, 15),
            Token::new("function", TokenType::Function, 22),
            Token::eof(30),
        ];
        assert_eq!(expected, actual);
    }
}
