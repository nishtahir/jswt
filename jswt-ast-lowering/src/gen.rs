use jswt_ast::*;
use jswt_common::Span;
// use jswt_parser::Parser;
// use jswt_tokenizer::Tokenizer;
use jswt_types::{PrimitiveType, Type};

// pub(crate) fn generate_expression(source: String) -> SourceElements {
//     let mut tokenizer = Tokenizer::default();
//     // Looks like leaking the source was predictably a terrible idea
//     // As a result of this we end up leaking sources we generate
//     tokenizer.enqueue_source_str("synthetic", Box::leak(source.into_boxed_str()));
//     let mut parser = Parser::new(&mut tokenizer);
//     let mut ast = parser.parse();
//     ast.program.files.pop().unwrap().source_elements
// }

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

pub(crate) fn function_1(name: &'static str, arg: i32) -> SingleExpression {
    SingleExpression::Arguments(ArgumentsExpression {
        span: Span::synthetic(),
        ident: Box::new(SingleExpression::Identifier(IdentifierExpression {
            span: Span::synthetic(),
            ident: Identifier::new(name, Span::synthetic()),
        })),
        arguments: ArgumentsList {
            span: Span::synthetic(),
            arguments: vec![SingleExpression::Literal(Literal::Integer(
                IntegerLiteral {
                    span: Span::synthetic(),
                    value: arg,
                },
            ))],
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
