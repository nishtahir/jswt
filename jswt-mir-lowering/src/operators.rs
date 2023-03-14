use jswt_ast::{
    transform::{walk_binary_expression, TransformVisitor},
    BinaryExpression,
};
use jswt_symbols::BindingsTable;

use crate::SymbolTable;

pub struct MirOperatorsLoweringContext<'a> {
    bindings: &'a BindingsTable,
    symbols: &'a SymbolTable,
}

impl<'a> MirOperatorsLoweringContext<'a> {
    pub fn new(bindings: &'a BindingsTable, symbols: &'a SymbolTable) -> Self {
        Self { bindings, symbols }
    }
}

impl<'a> TransformVisitor for MirOperatorsLoweringContext<'a> {
    fn visit_binary_expression(&mut self, node: &BinaryExpression) -> jswt_ast::SingleExpression {
        println!("Binary expression: {:?}", node);

        walk_binary_expression(self, node)
    }
}

#[cfg(test)]
mod test {

    use jswt_assert::assert_debug_snapshot;
    use jswt_parser::Parser;
    use jswt_semantics::GlobalResolver;
    use jswt_symbols::SymbolTable;
    use jswt_tokenizer::Tokenizer;

    use super::*;
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

        let mut symbol_table = SymbolTable::default();
        let mut bindings_table = BindingsTable::default();
        let mut global_resolver = GlobalResolver::new(&mut bindings_table, &mut symbol_table);
        global_resolver.resolve(&ast);

        // No errors in global resolver
        assert!(global_resolver.errors.len() == 0);

        let mut lowering = HirLoweringContext::new(&bindings_table, &symbol_table);
        let lowered = lowering.lower(&ast);
        assert_debug_snapshot!(lowered);
    }
}
