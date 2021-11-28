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

#[derive(Debug, PartialEq, Clone)]
pub enum SemanticError {
    VariableNotDefined {
        name: &'static str,
        span: Span,
    },
    VariableAlreadyDefined {
        name: &'static str,
        span: Span,
    },
    FunctionAlreadyDefined {
        name: &'static str,
        span: Span,
    },
    NotAFunctionError {
        span: Span,
        name_span: Span,
    },
    TypeError {
        span: Span,
        offending_token: Span,
        expected: &'static str,
    },
}

pub fn print_semantic_error(error: &SemanticError, source_map: &HashMap<String, &'static str>) {
    match error {
        SemanticError::VariableNotDefined { name, span } => {
            let file = &span.file;
            let source = source_map[file];
            let start = location_from_offset(source, span.start);
            let end = location_from_offset(source, span.end);
            println!("{}:{}:{} - error", file, start.line, start.col,);

            let error_span = NodeLocation { end, start };
            let frame = code_frame(
                source,
                error_span,
                &format!("Variable '{}' was not defined in this scope", name),
            );
            println!("{}\n", frame);
        }
        SemanticError::VariableAlreadyDefined { name, span } => {
            let file = &span.file;
            let source = source_map[file];
            let start = location_from_offset(source, span.start);
            let end = location_from_offset(source, span.end);
            println!("{}:{}:{} - error", file, start.line, start.col,);

            let error_span = NodeLocation { end, start };
            let frame = code_frame(
                source,
                error_span,
                &format!("Variable '{}' was already defined in this scope", name),
            );
            println!("{}\n", frame);
        }
        SemanticError::FunctionAlreadyDefined { name, span } => {
            let file = &span.file;
            let source = source_map[file];
            let start = location_from_offset(source, span.start);
            let end = location_from_offset(source, span.end);
            println!("{}:{}:{} - error", file, start.line, start.col,);

            let error_span = NodeLocation { end, start };
            let frame = code_frame(
                source,
                error_span,
                &format!("Function '{}' was already defined in this scope", name),
            );
            println!("{}\n", frame);
        }
        SemanticError::NotAFunctionError { span, name_span } => {
            let file = &span.file;
            let source = source_map[file];
            let start = location_from_offset(source, span.start);
            let end = location_from_offset(source, span.end);
            println!("{}:{}:{} - error", file, start.line, start.col,);

            let error_span = NodeLocation { end, start };

            let offending_token = &source[name_span.start..name_span.end];
            let frame = code_frame(
                source,
                error_span,
                &format!("'{}' is not a function.", offending_token),
            );
            println!("{}\n", frame);
        }
        SemanticError::TypeError {
            span,
            offending_token,
            expected,
        } => {
            let file = &span.file;
            let source = source_map[file];
            let start = location_from_offset(source, span.start);
            let end = location_from_offset(source, span.end);
            println!("{}:{}:{} - error", file, start.line, start.col,);

            let error_span = NodeLocation { end, start };

            let offending_token = &source[offending_token.start..offending_token.end];
            let frame = code_frame(
                source,
                error_span,
                &format!(
                    "Type Error: '{}' is not a {} expression.",
                    offending_token, expected
                ),
            );
            println!("{}\n", frame);
        }
    }
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

            println!("{}\n", frame);
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

            println!("{}\n", frame);
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

            println!("{}\n", frame);
        }
    }
}
