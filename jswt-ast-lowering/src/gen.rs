use jswt_ast::*;
use jswt_common::Span;
use jswt_types::{PrimitiveType, Type};

pub(crate) fn variable(name: &'static str, expression: SingleExpression) -> StatementElement {
    StatementElement::Variable(VariableStatement {
        span: Span::synthetic(),
        modifier: VariableModifier::Const(Span::synthetic()),
        target: AssignableElement::Identifier(Identifier::new(name, Span::synthetic())),
        expression,
        type_annotation: Some(TypeAnnotation {
            span: Span::synthetic(),
            ty: Type::Primitive(PrimitiveType::I32),
        }),
    })
}

pub(crate) fn function(name: &'static str, arguments: Vec<SingleExpression>) -> SingleExpression {
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

pub(crate) fn returns(expression: SingleExpression) -> StatementElement {
    StatementElement::Return(ReturnStatement {
        span: Span::synthetic(),
        expression,
    })
}

pub(crate) fn identifier(name: &'static str) -> SingleExpression {
    SingleExpression::Identifier(IdentifierExpression {
        span: Span::synthetic(),
        ident: Identifier::new(name, Span::synthetic()),
    })
}

pub(crate) fn malloc(size: i32) -> SingleExpression {
    function(
        "malloc",
        vec![SingleExpression::Literal(Literal::Integer(
            IntegerLiteral {
                span: Span::synthetic(),
                value: size,
            },
        ))],
    )
}

pub(crate) fn i32_store(target: &'static str, offset: i32, value: Identifier) -> SingleExpression {
    function(
        "i32Store",
        vec![
            SingleExpression::Additive(BinaryExpression {
                span: Span::synthetic(),
                left: Box::new(SingleExpression::Identifier(IdentifierExpression {
                    span: Span::synthetic(),
                    ident: Identifier::new(target, Span::synthetic()),
                })),
                op: BinaryOperator::And(Span::synthetic()),
                right: Box::new(SingleExpression::Literal(Literal::Integer(
                    IntegerLiteral {
                        span: Span::synthetic(),
                        value: offset,
                    },
                ))),
            }),
            SingleExpression::Identifier(IdentifierExpression {
                span: Span::synthetic(),
                ident: value,
            }),
        ],
    )
}

pub fn to_statement(expression: SingleExpression) -> StatementElement {
    StatementElement::Expression(ExpressionStatement {
        span: Span::synthetic(),
        expression,
    })
}
