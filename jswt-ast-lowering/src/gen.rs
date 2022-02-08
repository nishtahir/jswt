use jswt_ast::*;
use jswt_common::Span;
use jswt_parser::Parser;
use jswt_tokenizer::Tokenizer;

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

pub(crate) fn i32_store(
    target: &'static str,
    offset: i32,
    value: SingleExpression,
) -> SingleExpression {
    function(
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

pub fn parse_statement(expr: String) -> StatementElement {
    let mut tokenizer = Tokenizer::default();
    tokenizer.enqueue_source_str("synthetic", Box::leak(expr.into_boxed_str()));
    let mut parser = Parser::new(&mut tokenizer);
    parser.parse_statement()
}

pub fn parse_expr(expr: String) -> SingleExpression {
    let mut tokenizer = Tokenizer::default();
    tokenizer.enqueue_source_str("synthetic", Box::leak(expr.into_boxed_str()));
    let mut parser = Parser::new(&mut tokenizer);
    parser.single_expression().unwrap()
}
