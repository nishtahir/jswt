mod errors;
mod source;
mod token;

use lazy_static::lazy_static;
use regex::Regex;
use std::{
    cell::RefCell,
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
    rc::Rc,
};

pub use errors::TokenizerError;
pub use source::Source;
pub use token::{Token, TokenType};

// Utility to read a file from a given path
fn read_to_string(path: &Path) -> String {
    fs::read_to_string(&path).unwrap()
}

macro_rules! rules {
    ($($e:expr => $i:expr),*) => {
        {
            vec![$(
                TokenizerRule {
                    matcher: Regex::new($e).unwrap(),
                    token_type: $i,
                },
            )*]
        }
    };
}

macro_rules! directives {
    ($($e:expr => $i:expr),*) => {
        vec![
            $(
                TokenizerDirective {
                    matcher: Regex::new($e).unwrap(),
                    kind: $i,
                },
            )*
        ]
    };
}

#[derive(Debug, PartialEq)]
enum DirectiveType {
    Import,
    Skip,
}

struct TokenizerDirective {
    matcher: Regex,
    kind: DirectiveType,
}

struct TokenizerRule {
    matcher: Regex,
    token_type: TokenType,
}

// Regex::new can't be evaluated at compile time.
// Doesn't look like this will land anytime soon.
// https://github.com/rust-lang/regex/issues/607
lazy_static! {
    static ref DIRECTIVES: Vec<TokenizerDirective> = directives![
        // import "./test.jswt". Group 1 is the unquoted path
        r#"^\bimport\b\s+"((?:/)?(?:[^"]+(?:/)?)+)""# => DirectiveType::Import,
        r"^\s+" => DirectiveType::Skip,
        // Skip comments
        r"^//[^\n]*" => DirectiveType::Skip
    ];

    // https://docs.rs/regex/1.5.4/regex/struct.Regex.html#method.find
    // All searching is done with an implicit .*? at the beginning and end of an expression.
    // To force an expression to match the whole string (or a prefix or a suffix),
    // you must use an anchor like ^ or $ (or \A and \z)
    static ref RULES: Vec<TokenizerRule> = rules! [
        // Keywords
        r"^\btrue\b" => TokenType::True,
        r"^\bfalse\b" => TokenType::False,
        r"^\bprint\b" => TokenType::Print,
        r"^\bfunction\b" => TokenType::Function,
        r"^\bexport\b" => TokenType::Export,
        r"^\bimport\b" => TokenType::Import,
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
        r"^!=" => TokenType::BangEqual,
        r"^=" => TokenType::Equal,

        // Single character alternatives
        r"^\&" => TokenType::And,
        r"^\|" => TokenType::Or,
        r"^\~" => TokenType::Not,
        r"^\*" => TokenType::Star,
        r"^\\" => TokenType::Slash,
        r"^\+" => TokenType::Plus,
        r"^\-" => TokenType::Minus,
        r"^@" => TokenType::At,
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
        r#"^"[^"]*""# => TokenType::String
    ];
}

#[derive(Default)]
pub struct Tokenizer {
    source_map: HashMap<String, &'static str>,
    source_stack: Vec<Rc<RefCell<Source>>>,
    errors: Vec<TokenizerError>,
}

impl Tokenizer {
    pub fn next_token(&mut self) -> Option<Token> {
        if !self.has_more_sources() {
            return None;
        }

        let source = self.source_stack.last().unwrap().clone();
        let source = source.borrow();
        let offset = source.cursor();

        // If we've reached the end of a source file. Drop it from
        // our tokenization stack
        if !source.has_more_content() {
            self.pop_source();
            return self.next_token();
        }

        let rest = source.content_from_cursor();
        // Attempt to match the next token against tokenizer directives
        for directive in DIRECTIVES.iter() {
            if let Some(res) = directive.matcher.captures(rest) {
                // Group 0 is always the match text
                let match_text = res.get(0).unwrap().as_str();
                match directive.kind {
                    DirectiveType::Import => {
                        // Push the source where we found the import to the stack
                        self.push_source(res.get(1).unwrap().as_str());
                    }
                    DirectiveType::Skip => {}
                }

                // Skip the tokenizer directive by advancing the cursor.
                source.advance_cursor(match_text.len());
                return self.next_token();
            }
        }

        // Attempt to match the next token with a defined lexer rule
        for rule in RULES.iter() {
            if let Some(res) = rule.matcher.find(rest) {
                let match_text = res.as_str();
                // Advance cursor based on match
                source.advance_cursor(match_text.len());
                let token = Token::new(&source.name, match_text, rule.token_type, offset);
                return Some(token);
            }
        }

        // We want to report the error after the fact so note it down for now
        let err = TokenizerError::UnreconizedToken {
            file: source.name.to_owned(),
            offset,
            token: &rest[0..1],
        };

        self.errors.push(err);

        // Drop the offending token and move on to recognizing the next token
        source.advance_cursor(1);
        self.next_token()
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = vec![];
        while let Some(next) = self.next_token() {
            tokens.push(next);
        }
        tokens
    }

    pub fn push_source(&mut self, path: &str) {
        // Resolve the fully qualified path from relative paths
        // TODO - handle errors
        let path = fs::canonicalize(&PathBuf::from(path)).unwrap();
        let qualified_path = path.to_str().unwrap();

        // We're intentionally leaking this to make lifetime management easier
        // since we plan to pass references to the original sources in place of copying it
        let content = Box::leak(read_to_string(&path).into_boxed_str());
        self.push_source_str(qualified_path, content)
    }

    pub fn push_source_str(&mut self, path: &str, content: &'static str) {
        self.source_map.insert(path.to_owned(), content);
        let source = Source::new(path.to_string(), content);
        self.source_stack.push(Rc::new(RefCell::new(source)));
    }

    pub fn pop_source(&mut self) {
        self.source_stack.pop();
    }

    pub fn has_more_sources(&self) -> bool {
        !self.source_stack.is_empty()
    }

    /// Get a reference to the tokenizer's errors.
    pub fn errors(&self) -> Vec<TokenizerError> {
        self.errors.clone()
    }

    pub fn get_source(&self, path: &str) -> &'static str {
        self.source_map.get(path).unwrap()
    }

    /// Get a reference to the tokenizer's source map.
    pub fn source_map(&self) -> &HashMap<String, &'static str> {
        &self.source_map
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_tokenize_number() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "42");

        let actual = tokenizer.tokenize();
        let expected = vec![Token::new("test.1", "42", TokenType::Number, 0)];
        assert_eq!(expected, actual)
    }

    #[test]
    fn test_tokenize_numbers_with_whitespace() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "   4  2   ");

        let actual = tokenizer.tokenize();
        let expected = vec![
            Token::new("test.1", "4", TokenType::Number, 3),
            Token::new("test.1", "2", TokenType::Number, 6),
        ];
        assert_eq!(expected, actual)
    }

    #[test]
    fn test_tokenize_string() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "\"Hello World\"");

        let actual = tokenizer.tokenize();
        let expected = vec![Token::new(
            "test.1",
            "\"Hello World\"",
            TokenType::String,
            0,
        )];
        assert_eq!(expected, actual)
    }

    #[test]
    fn test_tokenize_braces() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "{}");

        let actual = tokenizer.tokenize();
        let expected = vec![
            Token::new("test.1", "{", TokenType::LeftBrace, 0),
            Token::new("test.1", "}", TokenType::RightBrace, 1),
        ];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenize_parens() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "()");

        let actual = tokenizer.tokenize();
        let expected = vec![
            Token::new("test.1", "(", TokenType::LeftParen, 0),
            Token::new("test.1", ")", TokenType::RightParen, 1),
        ];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenize_comma() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", ",");

        let actual = tokenizer.tokenize();
        let expected = vec![Token::new("test.1", ",", TokenType::Comma, 0)];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenize_less_than() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "<");

        let actual = tokenizer.tokenize();
        let expected = vec![Token::new("test.1", "<", TokenType::Less, 0)];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenize_less_than_equal() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "<=");

        let actual = tokenizer.tokenize();
        let expected = vec![Token::new("test.1", "<=", TokenType::LessEqual, 0)];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenize_greater_than() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", ">");

        let actual = tokenizer.tokenize();
        let expected = vec![Token::new("test.1", ">", TokenType::Greater, 0)];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenize_greater_than_equal() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", ">=");

        let actual = tokenizer.tokenize();
        let expected = vec![Token::new("test.1", ">=", TokenType::GreaterEqual, 0)];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenize_equal() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "=");

        let actual = tokenizer.tokenize();
        let expected = vec![Token::new("test.1", "=", TokenType::Equal, 0)];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenize_semi_colon() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", ";");

        let actual = tokenizer.tokenize();
        let expected = vec![Token::new("test.1", ";", TokenType::Semi, 0)];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_whitespace_generates_no_tokens() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "  ");

        let actual = tokenizer.tokenize();
        let expected: Vec<Token> = vec![];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenize_identifiers() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "user identifier");

        let actual = tokenizer.tokenize();
        let expected: Vec<Token> = vec![
            Token::new("test.1", "user", TokenType::Identifier, 0),
            Token::new("test.1", "identifier", TokenType::Identifier, 5),
        ];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenize_alphanumeric_identifiers() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "i32");

        let actual = tokenizer.tokenize();
        let expected: Vec<Token> = vec![Token::new("test.1", "i32", TokenType::Identifier, 0)];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenize_keywords() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "let false true return function");

        let actual = tokenizer.tokenize();
        let expected: Vec<Token> = vec![
            Token::new("test.1", "let", TokenType::Let, 0),
            Token::new("test.1", "false", TokenType::False, 4),
            Token::new("test.1", "true", TokenType::True, 10),
            Token::new("test.1", "return", TokenType::Return, 15),
            Token::new("test.1", "function", TokenType::Function, 22),
        ];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenize_let_assignment() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "let a = 99");

        let actual = tokenizer.tokenize();
        let expected: Vec<Token> = vec![
            Token::new("test.1", "let", TokenType::Let, 0),
            Token::new("test.1", "a", TokenType::Identifier, 4),
            Token::new("test.1", "=", TokenType::Equal, 6),
            Token::new("test.1", "99", TokenType::Number, 8),
        ];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_tokenize_annotation() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.push_source_str("test.1", "@test(\"(local.get 0)\")");

        let actual = tokenizer.tokenize();
        let expected: Vec<Token> = vec![
            Token::new("test.1", "@", TokenType::At, 0),
            Token::new("test.1", "test", TokenType::Identifier, 1),
            Token::new("test.1", "(", TokenType::LeftParen, 5),
            Token::new("test.1", "\"(local.get 0)\"", TokenType::String, 6),
            Token::new("test.1", ")", TokenType::RightParen, 21),
        ];
        assert_eq!(expected, actual);
    }
}
