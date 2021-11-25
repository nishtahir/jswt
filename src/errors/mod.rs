mod code;

use std::collections::HashMap;

pub use code::{code_frame, location_from_offset, Location, NodeLocation};

use crate::{ast::span::Span, tokenizer::TokenType};

#[derive(Debug, Clone)]
pub enum TokenizerError {
    UnreconizedToken {
        file: String,
        token: &'static str,
        offset: usize,
    },
    UnexpectedEof,
}

#[derive(Debug, Clone)]
pub enum ParseError {
    MismatchedToken {
        expected: TokenType,
        actual: TokenType,
        span: Span,
    },
    NoViableAlternative {
        expected: Vec<TokenType>,
        actual: TokenType,
        span: Span,
    },
}

pub fn print_tokenizer_error(error: &TokenizerError, source_map: &HashMap<String, &'static str>) {
    match error {
        TokenizerError::UnreconizedToken {
            file,
            token,
            offset,
        } => {
            let source = source_map[file];
            let location = location_from_offset(source, *offset);
            println!("{}:{}:{} - error", file, location.line, location.col,);

            let error_span = NodeLocation {
                end: Location {
                    line: location.line,
                    col: location.col + 1,
                },
                start: location,
            };
            let frame = code_frame(
                source,
                error_span,
                &format!("Unrecognized token '{}'", token),
            );

            println!("{}", frame);
        }
        TokenizerError::UnexpectedEof => todo!(),
    }
}

pub fn print_parser_error(error: &ParseError, source_map: &HashMap<String, &'static str>) {
    match error {
        ParseError::MismatchedToken {
            expected,
            actual,
            span,
        } => {
            let file = &span.file;
            let source = source_map[file];
            let start = location_from_offset(source, span.start);
            let end = location_from_offset(source, span.end);
            println!("{}:{}:{} - error", file, start.line, start.col,);

            let error_span = NodeLocation { end, start };
            let frame = code_frame(
                source,
                error_span,
                &format!(
                    "MismatchedToken: Expected {:?}, but found {:?}",
                    expected, actual
                ),
            );

            println!("{}", frame);
        }
        ParseError::NoViableAlternative {
            expected,
            actual,
            span,
        } => {
            let file = &span.file;
            let source = source_map[file];
            let start = location_from_offset(source, span.start);
            let end = location_from_offset(source, span.end);
            println!("{}:{}:{} - error", file, start.line, start.col,);

            let error_span = NodeLocation { end, start };
            let frame = code_frame(
                source,
                error_span,
                &format!(
                    "NoViableAlternative: Expected {:?}, but found {:?}",
                    expected, actual
                ),
            );

            println!("{}", frame);
        }
    }
}
