use regex::Regex;

use crate::errors::TokenizerError;
use crate::token::{Token, TokenType};

struct LexerRule {
    matcher: Regex,
    token_type: TokenType,
}

macro_rules! rules {
    ($($e:expr => $i:expr),*) => {
        {
            vec![$(
                LexerRule {
                    matcher: Regex::new($e).unwrap(),
                    token_type: $i,
                },
            )*]
        }
    };
}

pub struct Tokenizer<'a> {
    source: &'a str,
    cursor: usize,
    rules: Vec<LexerRule>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Tokenizer<'a> {
        Tokenizer {
            source: input,
            cursor: 0,
            // https://docs.rs/regex/1.5.4/regex/struct.Regex.html#method.find
            // All searching is done with an implicit .*? at the beginning and end of an expression.
            // To force an expression to match the whole string (or a prefix or a suffix),
            // you must use an anchor like ^ or $ (or \A and \z)
            rules: rules! {
                // Keywords
                r"^\btrue\b" => TokenType::True,
                r"^\bfalse\b" => TokenType::False,
                r"^\bprint\b" => TokenType::Print,
                r"^\bfunction\b" => TokenType::Function,
                r"^\bexport\b" => TokenType::Export,
                r"^\bif\b" => TokenType::If,
                r"^\belse\b" => TokenType::Else,
                r"^\breturn\b" => TokenType::Return,
                r"^\blet\b" => TokenType::Let,
                r"^\bconst\b" => TokenType::Const,

                // Multi character alternatives
                r"^<=" => TokenType::LessEqual,
                r"^<" => TokenType::Less,
                r"^>=" => TokenType::GreaterEqual,
                r"^>" => TokenType::Greater,
                r"^==" => TokenType::EqualEqual,
                r"^=" => TokenType::Equal,

                // Single character alternatives
                r"^," => TokenType::Comma,
                r"^:" => TokenType::Colon,
                r"^;" => TokenType::Semi,
                r"^\("=> TokenType::LeftParen,
                r"^\)"=> TokenType::RightParen,
                r"^\{"=> TokenType::LeftBrace,
                r"^\}"=> TokenType::RightBrace,

                // Multi character sequences
                r"^\d+" => TokenType::Number,
                r"^[_$a-zA-Z][_$a-zA-Z0-9]*" => TokenType::Identifier,
                r#"^"[^"]*""# => TokenType::String,

                // Other
                r"^//[^\n]*" => TokenType::Comment,
                r"^\s+" => TokenType::Skip
            },
        }
    }

    pub fn next_token(&mut self) -> Option<Token<'a>> {
        if !self.has_more_tokens() {
            return None;
        }
        // Attempt to match the next token with a defined lexer rule
        for rule in &self.rules {
            let offset = self.cursor;
            let rest = &self.source[offset..];
            if let Some(res) = rule.matcher.find(rest) {
                let match_text = res.as_str();
                // Advance cursor based on match
                self.cursor += match_text.len();
                if rule.token_type == TokenType::Skip {
                    return self.next_token();
                }
                let token = Token::new(match_text, rule.token_type, offset);
                return Some(token);
            }
        }
        None
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token<'a>>, TokenizerError> {
        let mut tokens = vec![];
        while let Some(next) = self.next_token() {
            tokens.push(next);
        }
        // If there are tokens we couldn't match
        if self.has_more_tokens() {
            return Err(TokenizerError::UnexpectedToken);
        }

        tokens.push(Token::eof(self.cursor));
        Ok(tokens)
    }

    pub fn has_more_tokens(&self) -> bool {
        self.cursor < self.source.len()
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
    fn test_tokenize_numbers_with_whitespace() {
        let mut tokenizer = Tokenizer::new("   4  2   ");
        let actual = tokenizer.tokenize().unwrap();
        let expected = vec![
            Token::new("4", TokenType::Number, 3),
            Token::new("2", TokenType::Number, 6),
            Token::eof(10),
        ];
        assert_eq!(expected, actual)
    }

    #[test]
    fn test_tokenize_string() {
        let mut tokenizer = Tokenizer::new("\"Hello World\"");
        let actual = tokenizer.tokenize().unwrap();
        let expected = vec![
            Token::new("\"Hello World\"", TokenType::String, 0),
            Token::eof(13),
        ];
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
        let expected = vec![Token::new(">", TokenType::Greater, 0), Token::eof(1)];
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
    fn test_tokenize_equal() {
        let mut tokenizer = Tokenizer::new("=");
        let actual = tokenizer.tokenize().unwrap();
        let expected = vec![Token::new("=", TokenType::Equal, 0), Token::eof(1)];
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
            Token::eof(25),
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
            Token::eof(15),
        ];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenize_alphanumeric_identifiers() {
        let mut tokenizer = Tokenizer::new("i32");
        let actual = tokenizer.tokenize().unwrap();
        let expected: Vec<Token> = vec![Token::new("i32", TokenType::Identifier, 0), Token::eof(3)];
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

    #[test]
    fn test_tokenize_let_assignment() {
        let mut tokenizer = Tokenizer::new("let a = 99");
        let actual = tokenizer.tokenize().unwrap();
        let expected: Vec<Token> = vec![
            Token::new("let", TokenType::Let, 0),
            Token::new("a", TokenType::Identifier, 4),
            Token::new("=", TokenType::Equal, 6),
            Token::new("99", TokenType::Number, 8),
            Token::eof(10),
        ];
        assert_eq!(expected, actual);
    }
}
