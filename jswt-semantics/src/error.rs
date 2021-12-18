use jswt_ast::high_level::BinaryOperator;
use jswt_common::{Span, Type};

#[derive(Debug, PartialEq)]
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
    TypeMismatchError {
        span: Span,
        expected: Type,
        actual: Type,
    },
    TypeParameterError {
        span: Span,
        parameter: Type,
        argument: Type,
    },
    TypeBinaryOperationError {
        span: Span,
        op: BinaryOperator,
        lhs: Type,
        rhs: Type,
    },
    TypeAssignmentError {
        span: Span,
        lhs: Type,
        rhs: Type,
    },
}
