use jswt_ast::{
    transform::{walk_binary_expression, TransformVisitor},
    ArgumentsExpression, ArgumentsList, BinaryExpression, BinaryOperator, MemberDotExpression,
    SingleExpression,
};
use jswt_common::{Spannable};
use jswt_synthetic::ident_exp;

pub struct HirOperatorsLoweringContext {}

impl HirOperatorsLoweringContext {
    pub fn new() -> Self {
        Self {}
    }
}

impl TransformVisitor for HirOperatorsLoweringContext {
    fn visit_binary_expression(&mut self, node: &BinaryExpression) -> SingleExpression {
        let left = self.visit_single_expression(&node.left);
        let right = self.visit_single_expression(&node.right);
        let ident = match node.op {
            // Arithmetic operators
            BinaryOperator::Plus(_) => "plus",
            BinaryOperator::Minus(_) => "minus",
            BinaryOperator::Mult(_) => "mult",
            BinaryOperator::Div(_) => "div",
            BinaryOperator::Equal(_) => "eq",
            // Comparison operators
            BinaryOperator::NotEqual(_)
            | BinaryOperator::Greater(_)
            | BinaryOperator::GreaterEqual(_)
            | BinaryOperator::Less(_)
            | BinaryOperator::LessEqual(_)
            | BinaryOperator::And(_)
            | BinaryOperator::Or(_) => {
                // TODO implement comparison operators
                return walk_binary_expression(self, node);
            }
            // Assignment operators
            BinaryOperator::Assign(_) => {
                // We can't rewrite assignment expressions into function calls
                return walk_binary_expression(self, node);
            }
        };

        SingleExpression::Arguments(ArgumentsExpression {
            span: node.span(),
            arguments: ArgumentsList {
                span: node.span(),
                arguments: vec![right],
            },
            ident: Box::new(SingleExpression::MemberDot(MemberDotExpression {
                span: node.span(),
                target: Box::new(left),
                expression: Box::new(ident_exp(ident.into(), node.span())),
            })),
        })
    }
}

#[cfg(test)]
mod test {

    use jswt_assert::assert_debug_snapshot;
    use jswt_parser::Parser;
    use jswt_semantics::GlobalSemanticResolver;
    use jswt_symbols::SemanticEnvironment;
    use jswt_tokenizer::Tokenizer;

    use crate::HirLoweringContext;

    #[test]
    fn test_operator_lowering_lowers_plus_symbol_into_function_call() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_operator_lowering_lowers_plus_symbol_into_function_call",
            r"
        class i32 {
            add(other: i32): i32 {
                return 0;
            }
        }

        function main(): i32 {
            let a: i32 = 1;
            let b: i32 = 2;
            let x = a + b;
            return 0;
        }
    ",
        );

        let ast = Parser::new(&mut tokenizer).parse();

        let mut environment = SemanticEnvironment::default();
        let mut global_resolver = GlobalSemanticResolver::new(&mut environment);
        global_resolver.resolve(&ast);

        // No errors in global resolver
        assert!(
            global_resolver.errors().len() == 0,
            "{:?}",
            global_resolver.errors()
        );

        let mut lowering = HirLoweringContext::new(&environment);
        let lowered = lowering.lower(&ast);
        assert_debug_snapshot!(lowered);
    }
}
