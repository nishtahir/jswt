mod codeframe;
mod emitter;
mod highlighter;

use emitter::ErrorEmitter;
use std::{borrow::Cow, collections::HashMap};

use jswt_common::Span;
use jswt_parser::ParseError;
use jswt_semantics::*;
use jswt_tokenizer::TokenizerError;

pub struct DiagnosticMessage {
    level: Level,
    message: Cow<'static, str>,
    span: Span,
    hint: Option<Cow<'static, str>>,
}

pub enum Level {
    Error,
    Warning,
}

pub fn print_semantic_error(error: &SemanticError, source_map: &HashMap<String, &'static str>) {
    let diagnostic = match error {
        SemanticError::VariableNotDefined { name, span } => DiagnosticMessage {
            level: Level::Error,
            span: span.clone(),
            message: format!("Variable '{}' was not defined in this scope", name).into(),
            hint: None,
        },
        SemanticError::VariableAlreadyDefined { name, span } => DiagnosticMessage {
            level: Level::Error,
            span: span.clone(),
            message: format!("Variable '{}' was already defined in this scope", name).into(),
            hint: None,
        },
        SemanticError::FunctionAlreadyDefined { name, span } => DiagnosticMessage {
            level: Level::Error,
            span: span.clone(),
            message: format!("Function '{}' was already defined in this scope", name).into(),
            hint: None,
        },
        SemanticError::NotAFunctionError { span, name_span } => {
            let file = &span.file.to_string();
            let source = source_map[file];
            let offending_token = &source[name_span.start..name_span.end];
            DiagnosticMessage {
                level: Level::Error,
                span: span.clone(),
                message: format!("'{}' is not a function.", offending_token).into(),
                hint: None,
            }
        }
        SemanticError::TypeError {
            span,
            offending_token,
            expected,
        } => {
            let file = &span.file.to_string();
            let source = source_map[file];
            let offending_token = &source[offending_token.start..offending_token.end];
            DiagnosticMessage {
                level: Level::Error,
                span: span.clone(),
                message: format!(
                    "TypeError: '{}' is not a '{}' expression.",
                    offending_token, expected
                )
                .into(),
                hint: None,
            }
        }
        SemanticError::FunctionNotDefined { span, name_span } => {
            let file = &span.file.to_string();
            let source = source_map[file];
            let offending_token = &source[name_span.start..name_span.end];
            DiagnosticMessage {
                level: Level::Error,
                span: span.clone(),
                message: format!("'{}' is not defined in this scope.", offending_token).into(),
                hint: None,
            }
        }
    };

    let emitter = ErrorEmitter::new(source_map);
    emitter.emit(&[diagnostic]);
}

pub fn print_tokenizer_error(error: &TokenizerError, source_map: &HashMap<String, &'static str>) {
    let diagnostic = match error {
        TokenizerError::UnreconizedToken {
            file,
            token,
            offset,
        } => DiagnosticMessage {
            level: Level::Error,
            span: Span::new(file.clone().into(),file.clone().into(), *offset, *offset + 1),
            message: format!("SyntaxError: Unrecognized token '{}'.", token).into(),
            hint: Some("Remove this token".into()),
        },
        TokenizerError::UnexpectedEof => todo!(),
    };
    let emitter = ErrorEmitter::new(source_map);
    emitter.emit(&[diagnostic]);
}

pub fn print_parser_error(error: &ParseError, source_map: &HashMap<String, &'static str>) {
    let diagnostic = match error {
        ParseError::MismatchedToken {
            expected,
            actual,
            span,
        } => DiagnosticMessage {
            level: Level::Error,
            span: span.clone(),
            message: format!(
                "MismatchedToken: Expected '{}', but found '{}'",
                expected, actual
            )
            .into(),
            hint: None,
        },

        ParseError::NoViableAlternative {
            expected,
            actual,
            span,
        } => DiagnosticMessage {
            level: Level::Error,
            span: span.clone(),
            message: format!(
                "NoViableAlternative: Expected {:?}, but found {:?}",
                expected, actual
            )
            .into(),
            hint: None,
        },
    };
    let emitter = ErrorEmitter::new(source_map);
    emitter.emit(&[diagnostic]);
}
