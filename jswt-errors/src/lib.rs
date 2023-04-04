mod codeframe;
mod emitter;
mod highlighter;

use emitter::ErrorEmitter;
use std::borrow::Cow;

use jswt_common::{fs, Span};
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

pub fn print_semantic_error(error: &SemanticError) {
    let diagnostic = match error {
        SemanticError::SymbolNotDefined { name, span } => DiagnosticMessage {
            level: Level::Error,
            span: span.clone(),
            message: format!("Symbol '{}' was not defined in this scope", name).into(),
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
        SemanticError::ClassAlreadyDefined { name, span } => DiagnosticMessage {
            level: Level::Error,
            span: span.clone(),
            message: format!("Class '{}' was already defined in this scope", name).into(),
            hint: None,
        },
        SemanticError::ClassNotDefined { ident, span } => DiagnosticMessage {
            level: Level::Error,
            span: span.clone(),
            message: format!("Class '{}' was not defined in this scope", ident).into(),
            hint: None,
        },
        SemanticError::NotAFunctionError { span, name_span } => {
            let file = &span.file.to_string();
            let source = fs::read_to_string(file);
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
            let source = fs::read_to_string(file);
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
            let source = fs::read_to_string(file);
            let offending_token = &source[name_span.start..name_span.end];
            DiagnosticMessage {
                level: Level::Error,
                span: span.clone(),
                message: format!("'{}' is not defined in this scope.", offending_token).into(),
                hint: None,
            }
        }
        SemanticError::ThisOutsideClass { span } => DiagnosticMessage {
            level: Level::Error,
            span: span.clone(),
            message: format!("'this' cannot be used outside a class definition.").into(),
            hint: None,
        },
        SemanticError::PropertyNotDefined { name, span } => DiagnosticMessage {
            level: Level::Error,
            span: span.clone(),
            message: format!("Property '{}' was not defined in this scope", name).into(),
            hint: None,
        },
        SemanticError::FieldAlreadyDefined { name, span } => DiagnosticMessage {
            level: Level::Error,
            span: span.clone(),
            message: format!("Field '{}' was already defined in this scope", name).into(),
            hint: None,
        },
        SemanticError::MethodAlreadyDefined { name, span } => DiagnosticMessage {
            level: Level::Error,
            span: span.clone(),
            message: format!("Method '{}' was already defined in this scope", name).into(),
            hint: None,
        },
        SemanticError::UnknownType { ident, span } => DiagnosticMessage {
            level: Level::Error,
            span: span.clone(),
            message: format!("Unable to determine type for '{}'", ident).into(),
            hint: None,
        },
        SemanticError::InvalidNewCall { span } => DiagnosticMessage {
            level: Level::Error,
            span: span.clone(),
            message: format!("'new' can only be used with a class.").into(),
            hint: None,
        },
    };

    let emitter = ErrorEmitter::new();
    emitter.emit(&[diagnostic]);
}

pub fn print_tokenizer_error(error: &TokenizerError) {
    let diagnostic = match error {
        TokenizerError::UnreconizedToken {
            file,
            token,
            offset,
        } => DiagnosticMessage {
            level: Level::Error,
            span: Span::new(
                file.clone().into(),
                file.clone().into(),
                *offset,
                *offset + 1,
            ),
            message: format!("SyntaxError: Unrecognized token '{}'.", token).into(),
            hint: Some("Remove this token".into()),
        },
        TokenizerError::UnexpectedEof => todo!(),
    };
    let emitter = ErrorEmitter::new();
    emitter.emit(&[diagnostic]);
}

pub fn print_parser_error(error: &ParseError) {
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
    let emitter = ErrorEmitter::new();
    emitter.emit(&[diagnostic]);
}
