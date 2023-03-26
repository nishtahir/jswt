use jswt_ast::{
    transform::{walk_assignment_expression, TransformVisitor},
    *,
};
use jswt_common::{Spannable, Typeable};
use jswt_synthetic::ident_exp;

// lowering context for member index expressions
pub struct HirMemberIndexContext {}

impl HirMemberIndexContext {
    pub fn new() -> Self {
        Self {}
    }
}

impl TransformVisitor for HirMemberIndexContext {
    fn visit_member_index(&mut self, node: &MemberIndexExpression) -> SingleExpression {
        // rewrite member index expressions into function call
        // we expect a member index expression to be of the form:
        // 'foo[bar]' to be rewritten into 'foo.get(bar)'
        // 'foo[bar] = baz' to be rewritten into 'foo.set(bar, baz)'
        let target = self.visit_single_expression(&node.target);
        let expression = self.visit_single_expression(&node.index);

        SingleExpression::MemberDot(MemberDotExpression {
            span: node.span(),
            target: Box::new(target),
            expression: Box::new(SingleExpression::Arguments(ArgumentsExpression {
                span: expression.span(),
                ty: expression.ty(),
                arguments: ArgumentsList {
                    span: expression.span(),
                    arguments: vec![expression],
                },
                ident: Box::new(ident_exp("get".into(), node.ty(), node.span())),
            })),
            ty: node.ty(),
        })
    }

    fn visit_assignment_expression(&mut self, node: &BinaryExpression) -> SingleExpression {
        // if the target is a member index expression we need to rewrite it
        // into a member dot expression with a function call
        if let SingleExpression::MemberIndex(index) = &*node.left {
            let target = self.visit_single_expression(&index.target);
            let expression = self.visit_single_expression(&index.index);

            return SingleExpression::MemberDot(MemberDotExpression {
                span: node.span(),
                target: Box::new(target),
                expression: Box::new(SingleExpression::Arguments(ArgumentsExpression {
                    span: expression.span(),
                    ty: expression.ty(),
                    arguments: ArgumentsList {
                        span: expression.span(),
                        arguments: vec![expression, self.visit_single_expression(&node.right)],
                    },
                    ident: Box::new(ident_exp("set".into(), node.ty(), node.span())),
                })),
                ty: node.ty(),
            });
        }
        walk_assignment_expression(self, node)
    }
}

#[cfg(test)]
mod test {
    use crate::HirLoweringContext;
    use jswt_assert::assert_debug_snapshot;
    use jswt_parser::Parser;
    use jswt_symbols::{BindingsTable, ScopedSymbolTable};
    use jswt_tokenizer::Tokenizer;

    #[test]
    fn test_member_index_get_is_lowered_into_get_call() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_member_index_get_is_lowered_into_get_call",
            r"
            function main() {
                let x = new Array();
                x[0];
            }
        ",
        );

        let ast = Parser::new(&mut tokenizer).parse();

        let symbol_table = ScopedSymbolTable::default();
        let bindings_table = BindingsTable::default();

        let mut lowering = HirLoweringContext::new(&bindings_table, &symbol_table);
        let lowered = lowering.lower(&ast);
        assert_debug_snapshot!(lowered);
    }
}
