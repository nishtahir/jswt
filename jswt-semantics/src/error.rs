use std::borrow::Cow;

use jswt_common::Span;

#[derive(Debug, PartialEq, Clone)]
pub enum SemanticError {
    VariableNotDefined {
        name: Cow<'static, str>,
        span: Span,
    },
    VariableAlreadyDefined {
        name: Cow<'static, str>,
        span: Span,
    },
    FunctionAlreadyDefined {
        name: Cow<'static, str>,
        span: Span,
    },
    ClassAlreadyDefined {
        name: Cow<'static, str>,
        span: Span,
    },
    FunctionNotDefined {
        name_span: Span,
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
