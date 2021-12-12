mod codeframe;
mod highlighter;

use colored::*;
use jswt_parser::ParseError;
use jswt_semantics::*;
use jswt_tokenizer::TokenizerError;
use std::collections::HashMap;

pub enum Level {
    Error,
    Warning,
}

use crate::{
    codeframe::{code_frame, location_from_offset, Location, NodeLocation},
    highlighter::highlight,
};

pub fn print_semantic_error(error: &SemanticError, source_map: &HashMap<String, &'static str>) {
    match error {
        SemanticError::VariableNotDefined { name, span } => {
            let file = &span.file.to_string();
            let source = source_map[file];
            let start = location_from_offset(source, span.start);
            let end = location_from_offset(source, span.end);
            let header = create_header(file, &start, Level::Error);

            let error_span = NodeLocation { end, start };
            let frame = create_codeframe(
                source,
                error_span,
                &format!("Variable '{}' was not defined in this scope", name),
            );
            println!("{}\n{}\n", header, frame);
        }
        SemanticError::VariableAlreadyDefined { name, span } => {
            let file = &span.file.to_string();
            let source = source_map[file];
            let start = location_from_offset(source, span.start);
            let end = location_from_offset(source, span.end);
            let header = create_header(file, &start, Level::Error);

            let error_span = NodeLocation { end, start };
            let frame = create_codeframe(
                source,
                error_span,
                &format!("Variable '{}' was already defined in this scope", name),
            );
            println!("{}\n{}\n", header, frame);
        }
        SemanticError::FunctionAlreadyDefined { name, span } => {
            let file = &span.file.to_string();
            let source = source_map[file];
            let start = location_from_offset(source, span.start);
            let end = location_from_offset(source, span.end);
            let header = create_header(file, &start, Level::Error);

            let error_span = NodeLocation { end, start };
            let frame = create_codeframe(
                source,
                error_span,
                &format!("Function '{}' was already defined in this scope", name),
            );
            println!("{}\n{}\n", header, frame);
        }
        SemanticError::NotAFunctionError { span, name_span } => {
            let file = &span.file.to_string();
            let source = source_map[file];
            let start = location_from_offset(source, span.start);
            let end = location_from_offset(source, span.end);
            let header = create_header(file, &start, Level::Error);
            let error_span = NodeLocation { end, start };
            let offending_token = &source[name_span.start..name_span.end];
            let frame = create_codeframe(
                source,
                error_span,
                &format!("'{}' is not a function.", offending_token),
            );
            println!("{}\n{}\n", header, frame);
        }
        SemanticError::TypeError {
            span,
            offending_token,
            expected,
        } => {
            let file = &span.file.to_string();
            let source = source_map[file];
            let start = location_from_offset(source, span.start);
            let end = location_from_offset(source, span.end);

            let header = create_header(file, &start, Level::Error);
            let error_span = NodeLocation { end, start };
            let offending_token = &source[offending_token.start..offending_token.end];
            let frame = create_codeframe(
                source,
                error_span,
                &format!(
                    "Type Error: '{}' is not a {} expression.",
                    offending_token, expected
                ),
            );
            println!("{}\n{}\n", header, frame);
        }
        SemanticError::FunctionNotDefined { span, name_span } => {
            let file = &span.file.to_string();
            let source = source_map[file];
            let start = location_from_offset(source, name_span.start);
            let end = location_from_offset(source, name_span.end);
            let header = create_header(file, &start, Level::Error);
            let error_span = NodeLocation { end, start };
            let offending_token = &source[name_span.start..name_span.end];
            let frame = create_codeframe(
                source,
                error_span,
                &format!("'{}' is not defined in this scope.", offending_token),
            );
            println!("{}\n{}\n", header, frame);
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

            let header = create_header(file, &location, Level::Error);
            let error_span = NodeLocation {
                end: Location {
                    line: location.line,
                    col: location.col + 1,
                },
                start: location,
            };

            let frame = create_codeframe(
                source,
                error_span,
                &format!("Unrecognized token '{}'", token),
            );

            println!("{}\n{}\n", header, frame);
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
            let file = &span.file.to_string();
            let source = source_map[file];
            let start = location_from_offset(source, span.start);
            let end = location_from_offset(source, span.end);

            let header = create_header(file, &start, Level::Error);
            let error_span = NodeLocation { end, start };
            let frame = create_codeframe(
                source,
                error_span,
                &format!(
                    "MismatchedToken: Expected '{}', but found '{}'",
                    expected, actual
                ),
            );

            println!("{}\n{}\n", header, frame);
        }
        ParseError::NoViableAlternative {
            expected,
            actual,
            span,
        } => {
            let file = &span.file.to_string();
            let source = source_map[file];
            let start = location_from_offset(source, span.start);
            let end = location_from_offset(source, span.end);

            let header = create_header(file, &start, Level::Error);
            let error_span = NodeLocation { end, start };
            let frame = create_codeframe(
                source,
                error_span,
                &format!(
                    "NoViableAlternative: Expected {:?}, but found {:?}",
                    expected, actual
                ),
            );
            println!("{}\n{}\n", header, frame);
        }
    }
}

fn create_header(file: &str, location: &Location, level: Level) -> String {
    let err = match level {
        Level::Error => "error".bright_red().bold(),
        Level::Warning => "warning".yellow().bold(),
    };

    format!("{}: {}:{}:{}", err, file, location.line, location.col)
}

fn create_codeframe(source: &str, location: NodeLocation, message: &str) -> String {
    code_frame(&highlight(source, false), location, message)
}
