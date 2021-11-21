use super::TokenizerRule;
use crate::token::Token;
use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
};

pub struct SourceReader {
    sources: HashMap<String, Source>,
    stack: RefCell<Vec<String>>,
}

impl<'a> SourceReader {
    pub fn new() -> Self {
        Self {
            sources: HashMap::new(),
            stack: RefCell::new(vec![]),
        }
    }

    /// Push a new source location to the stack
    pub fn push_source(&mut self, import_path: &str) {
        let source = Source {
            filename: import_path.to_string(), // FixMe: Don't Copy here
            path: import_path.to_string(),     // replace with qualified path,
            content: SourceReader::read_file(import_path).unwrap(),
            cursor: Cell::new(0),
        };

        self.sources.insert(import_path.to_string(), source);
        // Get a reference to the item in the list of sources
        // This way the lifetime of the item is tied to the
        // lifetime of the list
        self.stack.borrow_mut().push(import_path.to_string());
    }

    /// Pop source from the stack
    fn pop_source(&self) {
        self.stack.borrow_mut().pop();
    }

    fn advance_cursor(&self, amount: usize) {
        if let Some(source) = self.current_source() {
            source.advance_cursor(amount);
        }
    }

    fn current_source(&'a self) -> Option<&Source> {
        let stack = self.stack.borrow();
        let current_file = stack.last()?;
        self.sources.get(current_file)
    }

    pub fn next_chunk(&self) -> Option<&'a str> {
        let stack = self.stack.borrow();
        let current_file = stack.last()?;
        let source = self.sources.get(current_file)?;
        if source.has_more_content() {
            let content = source.content_from_offset();
            return Some(content)
        }



        self.pop_source();
        self.next_chunk()
    }

    /// Return the next chunk of source to the tokenizer
    /// starting at the given offset
    pub fn match_next_rule<'b>(&'b self, rule: &TokenizerRule) -> Option<Token<'b>> {
        let source = self.current_source()?;
        let content = source.content_from_offset();

        if let Some(res) = rule.matcher.find(content) {
            let match_text = res.as_str();
            let token = Token::new(match_text, rule.token_type, source.cursor());
            source.advance_cursor(match_text.len());
            return Some(token);
        }

        self.pop_source();
        self.match_next_rule(rule)
    }

    /// If we have a valid source that has content remaining
    pub fn has_more_tokens(&self) -> bool {
        let stack = self.stack.borrow();
        let s = stack.iter().rev().find(|s| {
            if let Some(source) = self.sources.get(*s) {
                return source.has_more_content();
            }
            return false;
        });
        s.is_some()
    }

    #[cfg(not(test))]
    fn read_file(path: &str) -> Result<String, std::io::Error> {
        std::fs::read_to_string(path)
    }

    #[cfg(test)]
    fn read_file(path: &str) -> Result<String, std::io::Error> {
        match path {
            "item.1" => Ok("let x = 1;".to_string()),
            _ => unreachable!(),
        }
    }
}

struct Source {
    filename: String,
    path: String,
    content: String,
    cursor: Cell<usize>,
}

impl Source {
    pub fn has_more_content(&self) -> bool {
        self.cursor.get() < self.content.len()
    }

    pub fn content_from_offset(&self) -> &str {
        &self.content[self.cursor.get()..]
    }

    pub fn advance_cursor(&self, amount: usize) {
        let current = self.cursor.get();
        self.cursor.set(current + amount);
    }

    fn cursor(&self) -> usize {
        self.cursor.get()
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::token::TokenType;
    use regex::Regex;

    #[test]
    fn test_source() {
        let rule = TokenizerRule {
            matcher: Regex::new(r"\w+").unwrap(),
            token_type: TokenType::Identifier,
        };

        let mut source_reader = SourceReader::new();
        source_reader.push_source("item.1");

        let next = source_reader.match_next_rule(&rule).unwrap();
        assert_eq!(Token::new("let", TokenType::Identifier, 0), next);
    }
}
