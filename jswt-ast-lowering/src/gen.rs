use std::borrow::Cow;

use jswt_ast::*;
use jswt_common::Span;

pub(crate) fn function_call(
    name: &'static str,
    arguments: Vec<SingleExpression>,
) -> SingleExpression {
    SingleExpression::Arguments(ArgumentsExpression {
        span: Span::synthetic(),
        ident: Box::new(SingleExpression::Identifier(IdentifierExpression {
            span: Span::synthetic(),
            ident: Identifier::new(name, Span::synthetic()),
        })),
        arguments: ArgumentsList {
            span: Span::synthetic(),
            arguments,
        },
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
                })),
                op: BinaryOperator::Plus(Span::synthetic()),
                right: Box::new(SingleExpression::Literal(Literal::Integer(
                    IntegerLiteral {
                        span: Span::synthetic(),
                        value: offset,
                    },
                ))),
            }),
            value,
        ],
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
            })),
            op: BinaryOperator::Plus(Span::synthetic()),
            right: Box::new(SingleExpression::Literal(Literal::Integer(
                IntegerLiteral {
                    span: Span::synthetic(),
                    value: offset,
                },
            ))),
        })],
    )
}

pub(crate) fn malloc(size: usize) -> SingleExpression {
    function_call(
        "malloc",
        vec![SingleExpression::Literal(Literal::Integer(
            IntegerLiteral {
                span: Span::synthetic(),
                value: size as i32,
            },
        ))],
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
    })
}

pub(crate) fn type_ptr() -> Type {
    Type::Identifier(IdentifierType { name: "ptr".into() })
}
