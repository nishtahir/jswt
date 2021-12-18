mod codeframe;
mod emitter;
mod highlighter;

use std::{borrow::Cow, collections::HashMap};

use jswt_common::Span;
use jswt_parser::ParseError;
use jswt_semantics::*;
use jswt_tokenizer::TokenizerError;

use crate::emitter::ErrorEmitter;

///
pub enum Level {
    Error,
    Warning,
}

///
pub struct DiagnosticMessage {
    level: Level,
    message: Cow<'static, str>,
    span: Span,
    hint: Option<Cow<'static, str>>,
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
        SemanticError::TypeMismatchError {
            span,
            expected,
            actual,
        } => DiagnosticMessage {
            level: Level::Error,
            span: span.clone(),
            message: format!(
                "TypeError: expected '{}' but found '{}'.",
                expected, actual,
            )
            .into(),
            hint: None,
        },
        SemanticError::TypeParameterError {
            span,
            parameter,
            argument,
        } => todo!(),
        SemanticError::TypeBinaryOperationError { span, op, lhs, rhs } => DiagnosticMessage {
            level: Level::Error,
            span: span.clone(),
            message: format!(
                "TypeError: '{:?}' cannot be applied to a {:?} and {:?}.",
                op, lhs, rhs
            )
            .into(),
            hint: None,
        },
        SemanticError::TypeAssignmentError { span, lhs, rhs } => DiagnosticMessage {
            level: Level::Error,
            span: span.clone(),
            message: format!(
                "TypeError: '{:?}' cannot be assigned to a {:?} expression.",
                lhs, rhs
            )
            .into(),
            hint: None,
        },
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
            span: Span::new(file.clone().into(), *offset, *offset + 1),
            message: format!("SyntaxError: Unrecognized token '{}'.", token).into(),
            hint: None,
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
