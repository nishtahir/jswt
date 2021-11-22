mod source;

use crate::error::TokenizerError;
use crate::token::{Token, TokenType};
use regex::Regex;
use std::cell::Cell;
use std::collections::HashMap;
use std::rc::Rc;

use self::source::Source;

#[derive(Debug, PartialEq)]
enum DirectiveType {
    Import,
    Skip,
}

struct TokenizerDirective {
    matcher: Regex,
    kind: DirectiveType,
}

pub struct TokenizerRule {
    matcher: Regex,
    token_type: TokenType,
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

macro_rules! rules {
    ($($e:expr => $i:expr),*) => {
        vec![
            $(
                TokenizerRule {
                    matcher: Regex::new($e).unwrap(),
                    token_type: $i,
                },
            )*
        ]
    };
}

// Regex::new can't be evaluated at compile time.
// Doesn't look like this will land anytime soon.
// https://github.com/rust-lang/regex/issues/607
lazy_static! {
    static ref DIRECTIVES: Vec<TokenizerDirective> = directives![
        // import "./test.jswt". Group 1 is the unquoted path
        r#"^\bimport\b\s+"((?:/)?(?:[^"]+(?:/)?)+)"\s"# => DirectiveType::Import,
        r"^\s+" => DirectiveType::Skip
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
        r"^//[^\n]*" => TokenType::Comment
    ];
}

pub struct Tokenizer {
    errors: Vec<TokenizerError>,
    stack: Rc<Vec<Source>>,
    source_map: HashMap<Rc<String>, Rc<String>>,
}

impl<'a> Tokenizer {
    pub fn new(path: &str) -> Self {
        let mut tokenizer = Self {
            errors: vec![],
            stack: Rc::new(vec![]),
            source_map: HashMap::new(),
        };
        // Seed the initial file to begin tokenizing
        tokenizer.push_source(path);
        tokenizer
    }

    pub fn next_token(&mut self) -> Option<Token> {
        if self.stack.is_empty() {
            return None;
        }

        let stack = self.stack.clone();
        let source_file = stack.last()?;
        if !source_file.has_more() {
            self.pop_source();
            return self.next_token();
        }

        let offset = source_file.cursor();
        let rest = source_file.next_chunk();

        // Attempt to match the next token against tokenizer directives
        for directive in DIRECTIVES.iter() {
            if let Some(res) = directive.matcher.captures(rest) {
                // Group 0 is always the match text
                let match_text = res.get(0).unwrap().as_str();

                match directive.kind {
                    // Push a
                    DirectiveType::Import => {
                        let path = res.get(1).unwrap().as_str();
                        // We don't want to advance here. We want Tokenize the to parse the
                        self.push_source(path);
                        break;
                    }
                    DirectiveType::Skip => {
                        // Skip the token by advancing the cursor
                        source_file.advance_by(match_text.len());
                        return self.next_token();
                    }
                }
                // Skip to the next token
            }
        }

        // Attempt to match the next token with a defined lexer rule
        for rule in RULES.iter() {
            if let Some(res) = rule.matcher.find(rest) {
                let match_text = res.as_str();
                // Advance cursor based on match
                let token = Token::new(
                    source_file.name.clone(),
                    rule.token_type,
                    offset,
                    match_text.len(),
                );
                source_file.advance_by(match_text.len());
                return Some(token);
            }
        }

        // We couldn't find a match for this
        None
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, TokenizerError> {
        let mut tokens = vec![];
        while let Some(next) = self.next_token() {
            tokens.push(next);
        }
        // If there are tokens we couldn't match
        if self.has_more_tokens() {
            return Err(TokenizerError::UnexpectedToken(0));
        }

        Ok(tokens)
    }

    pub fn has_more_tokens(&self) -> bool {
        // self.reader.has_more_tokens()
        false
    }

    /// Get a reference to the tokenizer's errors.
    pub fn errors(&self) -> &[TokenizerError] {
        self.errors.as_ref()
    }

    fn push_source(&mut self, path: &str) {
        println!("{}", path);
        let content = Tokenizer::read_file(&path).unwrap();
        let key = Rc::new(path.to_owned());
        let value = Rc::new(content);

        // The source map is acting as the owning data structure for the
        // file content.
        self.source_map.insert(key.clone(), value.clone());

        let source = Source {
            name: key.clone(), // FixMe: Don't Copy here
            cursor: Cell::new(0),
            content: value.clone(),
        };
        // // Get a reference to the item in the list of sources
        // // This way the lifetime of the item is tied to the
        // // lifetime of the list
        Rc::get_mut(&mut self.stack).unwrap().push(source);
    }

    fn pop_source(&mut self) {
        Rc::get_mut(&mut self.stack).unwrap().pop();
    }

    fn read_file(path: &str) -> Result<String, std::io::Error> {
        std::fs::read_to_string(path)
    }
}

// #[cfg(test)]
// mod test {

//     use super::*;

//     #[test]
//     fn test_tokenize_number() {
//         let mut tokenizer = Tokenizer::new("42");
//         let actual = tokenizer.tokenize().unwrap();
//         let expected = vec![Token::new("42", TokenType::Number, 0), Token::eof(2)];
//         assert_eq!(expected, actual)
//     }

//     #[test]
//     fn test_tokenize_numbers_with_whitespace() {
//         let mut tokenizer = Tokenizer::new("   4  2   ");
//         let actual = tokenizer.tokenize().unwrap();
//         let expected = vec![
//             Token::new("4", TokenType::Number, 3),
//             Token::new("2", TokenType::Number, 6),
//             Token::eof(10),
//         ];
//         assert_eq!(expected, actual)
//     }

//     #[test]
//     fn test_tokenize_string() {
//         let mut tokenizer = Tokenizer::new("\"Hello World\"");
//         let actual = tokenizer.tokenize().unwrap();
//         let expected = vec![
//             Token::new("\"Hello World\"", TokenType::String, 0),
//             Token::eof(13),
//         ];
//         assert_eq!(expected, actual)
//     }

//     #[test]
//     fn test_tokenize_braces() {
//         let mut tokenizer = Tokenizer::new("{}");
//         let actual = tokenizer.tokenize().unwrap();
//         let expected = vec![
//             Token::new("{", TokenType::LeftBrace, 0),
//             Token::new("}", TokenType::RightBrace, 1),
//             Token::eof(2),
//         ];
//         assert_eq!(expected, actual);
//     }

//     #[test]
//     fn test_tokenize_parens() {
//         let mut tokenizer = Tokenizer::new("()");
//         let actual = tokenizer.tokenize().unwrap();
//         let expected = vec![
//             Token::new("(", TokenType::LeftParen, 0),
//             Token::new(")", TokenType::RightParen, 1),
//             Token::eof(2),
//         ];
//         assert_eq!(expected, actual);
//     }

//     #[test]
//     fn test_tokenize_comma() {
//         let mut tokenizer = Tokenizer::new(",");
//         let actual = tokenizer.tokenize().unwrap();
//         let expected = vec![Token::new(",", TokenType::Comma, 0), Token::eof(1)];
//         assert_eq!(expected, actual);
//     }

//     #[test]
//     fn test_tokenize_less_than() {
//         let mut tokenizer = Tokenizer::new("<");
//         let actual = tokenizer.tokenize().unwrap();
//         let expected = vec![Token::new("<", TokenType::Less, 0), Token::eof(1)];
//         assert_eq!(expected, actual);
//     }

//     #[test]
//     fn test_tokenize_less_than_equal() {
//         let mut tokenizer = Tokenizer::new("<=");
//         let actual = tokenizer.tokenize().unwrap();
//         let expected = vec![Token::new("<=", TokenType::LessEqual, 0), Token::eof(2)];
//         assert_eq!(expected, actual);
//     }
//     #[test]
//     fn test_tokenize_greater_than() {
//         let mut tokenizer = Tokenizer::new(">");
//         let actual = tokenizer.tokenize().unwrap();
//         let expected = vec![Token::new(">", TokenType::Greater, 0), Token::eof(1)];
//         assert_eq!(expected, actual);
//     }
//     #[test]
//     fn test_tokenize_greater_than_equal() {
//         let mut tokenizer = Tokenizer::new(">=");
//         let actual = tokenizer.tokenize().unwrap();
//         let expected = vec![Token::new(">=", TokenType::GreaterEqual, 0), Token::eof(2)];
//         assert_eq!(expected, actual);
//     }

//     #[test]
//     fn test_tokenize_equal() {
//         let mut tokenizer = Tokenizer::new("=");
//         let actual = tokenizer.tokenize().unwrap();
//         let expected = vec![Token::new("=", TokenType::Equal, 0), Token::eof(1)];
//         assert_eq!(expected, actual);
//     }

//     #[test]
//     fn test_tokenize_semi_colon() {
//         let mut tokenizer = Tokenizer::new(";");
//         let actual = tokenizer.tokenize().unwrap();
//         let expected = vec![Token::new(";", TokenType::Semi, 0), Token::eof(1)];
//         assert_eq!(expected, actual);
//     }

//     #[test]
//     fn test_tokenize_comment() {
//         let mut tokenizer = Tokenizer::new("// This is a test comment");
//         let actual = tokenizer.tokenize().unwrap();
//         let expected = vec![
//             Token::new("// This is a test comment", TokenType::Comment, 0),
//             Token::eof(25),
//         ];
//         assert_eq!(expected, actual);
//     }

//     #[test]
//     fn test_whitespace_generates_no_tokens() {
//         let mut tokenizer = Tokenizer::new("  ");
//         let actual = tokenizer.tokenize().unwrap();
//         let expected: Vec<Token> = vec![Token::eof(2)];
//         assert_eq!(expected, actual);
//     }

//     #[test]
//     fn test_tokenize_identifiers() {
//         let mut tokenizer = Tokenizer::new("user identifier");
//         let actual = tokenizer.tokenize().unwrap();
//         let expected: Vec<Token> = vec![
//             Token::new("user", TokenType::Identifier, 0),
//             Token::new("identifier", TokenType::Identifier, 5),
//             Token::eof(15),
//         ];
//         assert_eq!(expected, actual);
//     }

//     #[test]
//     fn test_tokenize_alphanumeric_identifiers() {
//         let mut tokenizer = Tokenizer::new("i32");
//         let actual = tokenizer.tokenize().unwrap();
//         let expected: Vec<Token> = vec![Token::new("i32", TokenType::Identifier, 0), Token::eof(3)];
//         assert_eq!(expected, actual);
//     }

//     #[test]
//     fn test_tokenize_keywords() {
//         let mut tokenizer = Tokenizer::new("let false true return function");
//         let actual = tokenizer.tokenize().unwrap();
//         let expected: Vec<Token> = vec![
//             Token::new("let", TokenType::Let, 0),
//             Token::new("false", TokenType::False, 4),
//             Token::new("true", TokenType::True, 10),
//             Token::new("return", TokenType::Return, 15),
//             Token::new("function", TokenType::Function, 22),
//             Token::eof(30),
//         ];
//         assert_eq!(expected, actual);
//     }

//     #[test]
//     fn test_tokenize_let_assignment() {
//         let mut tokenizer = Tokenizer::new("let a = 99");
//         let actual = tokenizer.tokenize().unwrap();
//         let expected: Vec<Token> = vec![
//             Token::new("let", TokenType::Let, 0),
//             Token::new("a", TokenType::Identifier, 4),
//             Token::new("=", TokenType::Equal, 6),
//             Token::new("99", TokenType::Number, 8),
//             Token::eof(10),
//         ];
//         assert_eq!(expected, actual);
//     }

//     #[test]
//     fn test_tokenize_import_directive() {
//         let mut tokenizer = Tokenizer::new("import \"./string.jswt\"");
//         let actual = tokenizer.tokenize().unwrap();
//         let expected: Vec<Token> = vec![];
//         assert_eq!(expected, actual);
//     }
// }
