use std::borrow::Cow;

use jswt_ast::*;
use jswt_common::{Span, Type};

pub(crate) fn function_call(
    name: &'static str,
    arguments: Vec<SingleExpression>,
    ret: Type,
) -> SingleExpression {
    SingleExpression::Arguments(ArgumentsExpression {
        span: Span::synthetic(),
        ident: Box::new(SingleExpression::Identifier(IdentifierExpression {
            span: Span::synthetic(),
            ident: Identifier::new(name, Span::synthetic()),
            ty: ret.clone(),
        })),
        arguments: ArgumentsList {
            span: Span::synthetic(),
            arguments,
        },
        ty: ret,
    })
}

pub(crate) fn i32_store(
    target: &'static str,
    offset: i32,
    value: SingleExpression,
) -> SingleExpression {
    function_call(
        "i32Store",
        vec![
            SingleExpression::Additive(BinaryExpression {
                span: Span::synthetic(),
                left: Box::new(SingleExpression::Identifier(IdentifierExpression {
                    span: Span::synthetic(),
                    ident: Identifier::new(target, Span::synthetic()),
                    ty: type_i32(),
                })),
                op: BinaryOperator::Plus(Span::synthetic()),
                right: Box::new(SingleExpression::Literal(Literal::Integer(
                    IntegerLiteral {
                        span: Span::synthetic(),
                        value: offset,
                        ty: type_i32(),
                    },
                ))),
                ty: type_i32(),
            }),
            value,
        ],
        type_i32(),
    )
}

pub(crate) fn i32_load(target: &'static str, offset: i32) -> SingleExpression {
    function_call(
        "i32Load",
        vec![SingleExpression::Additive(BinaryExpression {
            span: Span::synthetic(),
            left: Box::new(SingleExpression::Identifier(IdentifierExpression {
                span: Span::synthetic(),
                ident: Identifier::new(target, Span::synthetic()),
                ty: type_ptr(),
            })),
            op: BinaryOperator::Plus(Span::synthetic()),
            right: Box::new(SingleExpression::Literal(Literal::Integer(
                IntegerLiteral {
                    span: Span::synthetic(),
                    value: offset,
                    ty: type_i32(),
                },
            ))),
            ty: type_i32(),
        })],
        type_i32(),
    )
}

pub(crate) fn malloc(size: usize) -> SingleExpression {
    function_call(
        "malloc",
        vec![SingleExpression::Literal(Literal::Integer(
            IntegerLiteral {
                span: Span::synthetic(),
                value: size as i32,
                ty: type_ptr(),
            },
        ))],
        type_ptr(),
    )
}

pub(crate) fn variable_decl_stmt(
    ident: Cow<'static, str>,
    expression: SingleExpression,
) -> StatementElement {
    StatementElement::Variable(VariableStatement {
        span: Span::synthetic(),
        modifier: VariableModifier::Const(Span::synthetic()),
        target: AssignableElement::Identifier(Identifier {
            span: Span::synthetic(),
            value: ident,
        }),
        expression,
        type_annotation: None,
    })
}

pub(crate) fn return_stmt(expression: SingleExpression) -> StatementElement {
    StatementElement::Return(ReturnStatement {
        span: Span::synthetic(),
        expression,
    })
}

pub(crate) fn ident_exp(ident: Cow<'static, str>) -> SingleExpression {
    SingleExpression::Identifier(IdentifierExpression {
        span: Span::synthetic(),
        ident: Identifier {
            span: Span::synthetic(),
            value: ident,
        },
        ty: type_ptr(),
    })
}

pub(crate) fn type_ptr() -> Type {
    Type::Binding("ptr".into())
}

pub(crate) fn type_i32() -> Type {
    Type::Binding("i32".into())
}