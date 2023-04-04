use std::borrow::Cow;

use jswt_ast::*;
use jswt_common::{Span, Type};

pub fn function_call(
    name: Cow<'static, str>,
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

pub fn i32_store(target: &'static str, offset: i32, value: SingleExpression) -> SingleExpression {
    function_call(
        "i32Store".into(),
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

pub fn i32_load(target: &'static str, offset: i32) -> SingleExpression {
    function_call(
        "i32Load".into(),
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

pub fn malloc(size: usize) -> SingleExpression {
    function_call(
        "malloc".into(),
        vec![SingleExpression::Literal(Literal::Integer(
            IntegerLiteral {
                span: Span::synthetic(),
                value: size as i32,
            },
        ))],
    )
}

pub fn variable_decl_stmt(
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

pub fn return_stmt(expression: SingleExpression) -> StatementElement {
    StatementElement::Return(ReturnStatement {
        span: Span::synthetic(),
        expression,
    })
}

pub fn ident_exp(ident: Cow<'static, str>, span: Span) -> SingleExpression {
    SingleExpression::Identifier(IdentifierExpression {
        span: span.clone(),
        ident: Identifier { span, value: ident },
    })
}

pub fn type_ptr() -> Type {
    Type::Binding("ptr".into())
}

pub fn type_i32() -> Type {
    Type::Binding("i32".into())
}
