use jswt_ast::Span;

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
