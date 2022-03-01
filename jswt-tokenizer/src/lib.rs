mod errors;
mod source;
mod token;

use lazy_static::lazy_static;
use regex::Regex;
use std::{
    cell::RefCell,
    path::{Path, PathBuf},
    process,
    rc::Rc,
};

use jswt_common::{fs, Span};

pub use errors::TokenizerError;
pub use source::Source;
pub use token::{Token, TokenType};

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
        //https://docs.rs/regex/latest/regex/#grouping-and-flags
        r"(?s)^/\*.*?\*/" => DirectiveType::Skip,
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
        r"^\bfunction\b" => TokenType::Function,
        r"^\bexport\b" => TokenType::Export,
        r"^\bimport\b" => TokenType::Import,
        r"^\bif\b" => TokenType::If,
        r"^\belse\b" => TokenType::Else,
        r"^\breturn\b" => TokenType::Return,
        r"^\blet\b" => TokenType::Let,
        r"^\bconst\b" => TokenType::Const,
        r"^\bwhile\b" => TokenType::While,
        r"^\bclass\b" => TokenType::Class,
        r"^\bconstructor\b" => TokenType::Constructor,
        r"^\bthis\b" => TokenType::This,
        r"^\bnew\b" => TokenType::New,

        // Multi character alternatives
        r"^\+\+" => TokenType::PlusPlus,
        r"^\-\-" => TokenType::MinusMinus,
        r"^\+" => TokenType::Plus,
        r"^\-" => TokenType::Minus,
        r"^<=" => TokenType::LessEqual,
        r"^<" => TokenType::Less,
        r"^>=" => TokenType::GreaterEqual,
        r"^>" => TokenType::Greater,
        r"^==" => TokenType::EqualEqual,
        r"^!=" => TokenType::BangEqual,
        r"^=" => TokenType::Equal,

        // Single character alternatives
        r"^\." => TokenType::Dot,
        r"^\&" => TokenType::And,
        r"^\|" => TokenType::Or,
        r"^\~" => TokenType::Not,
        r"^\*" => TokenType::Star,
        r"^/" => TokenType::Slash,
        r"^@" => TokenType::At,
        r"^," => TokenType::Comma,
        r"^:" => TokenType::Colon,
        r"^;" => TokenType::Semi,
        r"^\("=> TokenType::LeftParen,
        r"^\)"=> TokenType::RightParen,
        r"^\{"=> TokenType::LeftBrace,
        r"^\}"=> TokenType::RightBrace,
        r"^\["=> TokenType::LeftBracket,
        r"^\]"=> TokenType::RightBracket,

        // Multi character sequences
        r"^0[xX][0-9a-fA-F]+" => TokenType::HexInteger,
        r"^\d+\.\d+" => TokenType::Float,
        r"^\d+" => TokenType::Integer,
        r"^[_$a-zA-Z][_$a-zA-Z0-9]*" => TokenType::Identifier,
        r#"^"[^"]*""# => TokenType::String,

        // Skip comments
        r"^\s+" => TokenType::WhiteSpace
    ];
}

pub struct Tokenizer {
    /// We're using a vec here as a queue of sources
    /// to be tokenized. the current source being parsed should be
    /// at the end of the Vec to be popped once it's completely tokenized
    sources: Vec<Rc<RefCell<Source>>>,
    errors: Vec<TokenizerError>,
    // Root directory to compute relative paths from
    // This will be used together with the give module prefix
    // To compute a module name for the source file
    sources_root: Option<PathBuf>,
    module_prefix: Option<String>,
}

impl Default for Tokenizer {
    fn default() -> Self {
        Tokenizer::new()
    }
}

impl Tokenizer {
    pub fn new() -> Self {
        Self {
            sources: vec![],
            errors: vec![],
            sources_root: None,
            module_prefix: None,
        }
    }

    /// Get a reference to the tokenizer's errors.
    pub fn errors(&self) -> Vec<TokenizerError> {
        self.errors.clone()
    }

    pub fn next_token(&mut self) -> Option<Token> {
        if !self.has_more_sources() {
            return None;
        }

        let source = self.sources.last().unwrap().clone();
        let source = source.borrow();
        let offset = source.cursor();

        // If we've reached the end of a source file. Drop it from
        // our tokenization stack
        if !source.has_more_content() {
            self.dequeue_source();
            return Some(Token::new(
                Span::new(source.path.clone(), source.module.clone(), offset, offset),
                TokenType::Eof,
            ));
        }

        let rest = source.content_from_cursor();
        // Attempt to match the next token against tokenizer directives
        for directive in DIRECTIVES.iter() {
            if let Some(res) = directive.matcher.captures(rest) {
                // Group 0 is always the match text
                let match_text = res.get(0).unwrap().as_str();
                match directive.kind {
                    DirectiveType::Import => {
                        let import_path = res.get(1).unwrap().as_str();

                        // Construct path relative to the import file directory
                        // as opposed to using pwd as the root path for imports
                        let source_path = PathBuf::from(source.path.to_string());
                        let source_dir = source_path.parent().unwrap();
                        let relative_source_path = PathBuf::from(format!(
                            "{}/{}",
                            source_dir.to_str().unwrap(),
                            import_path
                        ));

                        // Push the source where we found the import to the queue
                        self.enqueue_source_file(&relative_source_path);
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
                let len = res.as_str().len();
                // Advance cursor based on match
                source.advance_cursor(len);
                let token = Token::new(
                    Span::new(
                        source.path.clone(),
                        source.module.clone(),
                        offset,
                        offset + len,
                    ),
                    rule.token_type,
                );
                return Some(token);
            }
        }

        // We want to report the error after the fact so note it down for now
        let err = TokenizerError::UnreconizedToken {
            file: source.path.clone().into(),
            offset,
            token: rest[0..1].to_string().into(),
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

    pub fn enqueue_source_file(&mut self, path: &Path) {
        let path = fs::canonicalize(path).unwrap();
        if !path.exists() {
            // TODO handle gracefully
            println!("No such file '{:?}'", path.to_str());
            process::exit(1);
        }

        let path = fs::canonicalize(path).unwrap();
        let qualified_path = path.to_str().unwrap();
        if !fs::is_cached(qualified_path) {
            // No need to cache the content up front.
            // As long as it exists it will be loaded lazily
            self.enqueue_source(qualified_path)
        }
    }

    /// Add a source to the queue to be parsed
    pub fn enqueue_source_str<T: AsRef<str>>(&mut self, path: &str, content: T) {
        // Cache the content here because we don't know if we'll get
        // access to the content later
        fs::cache(path, content.as_ref().to_owned());
        self.enqueue_source(path)
    }

    pub fn enqueue_source(&mut self, path: &str) {
        // Compute the module name based on the given source roots
        // and module prefix or use defaults
        let default_source_root = &std::env::current_dir().unwrap();
        let default_module_prefix = "module";
        let sources_root = self.sources_root.as_ref().unwrap_or(default_source_root);
        let module_prefix = self
            .module_prefix
            .as_deref()
            .unwrap_or(default_module_prefix);

        let relative_diff = pathdiff::diff_paths(path, sources_root);
        let module_name = if let Some(diff) = relative_diff {
            format!("{}/{}", module_prefix, diff.to_str().unwrap())
        } else {
            // We couldn't figure out a module name for this
            // So just use the path
            format!("{}/{}", module_prefix, path)
        };

        let source = Source::new(path.to_owned().into(), module_name.into());
        self.sources.insert(0, Rc::new(RefCell::new(source)));
    }

    pub fn set_sources_root(&mut self, path: Option<&PathBuf>) {
        self.sources_root = path.map(PathBuf::clone)
    }

    pub fn set_module_prefix(&mut self, prefix: Option<String>) {
        self.module_prefix = prefix;
    }

    fn dequeue_source(&mut self) {
        self.sources.pop();
    }

    fn has_more_sources(&self) -> bool {
        !self.sources.is_empty()
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use jswt_assert::assert_debug_snapshot;

    #[test]
    fn test_tokenize_number() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_tokenize_number", "42");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_tokenize_numbers_with_whitespace() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_tokenize_numbers_with_whitespace", "   4  2   ");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_tokenize_string() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_tokenize_string", "\"Hello World\"");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_tokenize_braces() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_tokenize_braces", "{}");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_tokenize_parens() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_tokenize_parens", "()");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_tokenize_comma() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_tokenize_comma", ",");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_tokenize_less_than() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_tokenize_less_than", "<");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_tokenize_less_than_equal() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_tokenize_less_than_equal", "<=");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_tokenize_greater_than() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_tokenize_greater_than", ">");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_tokenize_greater_than_equal() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_tokenize_greater_than_equal", ">=");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_tokenize_equal() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_tokenize_equal", "=");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_tokenize_semi_colon() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_tokenize_semi_colon", ";");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_whitespace_generates_no_tokens() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_whitespace_generates_no_tokens", "  ");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_tokenize_identifiers() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_tokenize_identifiers", "user identifier");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_tokenize_alphanumeric_identifiers() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_tokenize_alphanumeric_identifiers", "i32");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_tokenize_keywords() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_tokenize_keywords", "let false true return function");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_tokenize_let_assignment() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_tokenize_let_assignment", "let a = 99");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_tokenize_annotation() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_tokenize_annotation", "@test(\"(local.get 0)\")");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_tokenize_if_statement() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_tokenize_if_statement", "if(x == y) { return 0; } else { return 1; }");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_tokenize_if_else_if_statement() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_tokenize_if_else_if_statement", "if(x == y) { return 0; } else { return 1; }");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_tokenize_while_loop() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_tokenize_while_loop", "while(x == 99) { print(\"test\"); }");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_tokenize_hexadecimal_number() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_tokenize_hexadecimal_number", "0xABCD01234");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_tokenize_comments() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_tokenize_comments", "/* content more \n content */");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_tokenize_array() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_tokenize_array", "let arr = [1, 2, 3];");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_mutiline_comment_is_non_greedy() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_mutiline_comment_is_non_greedy",
            "/* comment */let arr = [1, 2, 3]; /** comment 2 */ let arr = [1, 2, 3];",
        );
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_tokenize_plus_plus_and_minus_minus() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_tokenize_plus_plus_and_minus_minus", "x++; y--");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }

    #[test]
    fn test_tokenize_class_declaration() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str("test_tokenize_class_declaration", "class A { constructor(b: i32) {} }");
        let actual = tokenizer.tokenize();
        assert!(tokenizer.errors().is_empty());
        assert_debug_snapshot!(actual);
    }
}
