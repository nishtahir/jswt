use jswt_ast::{transform::TransformVisitor, NewExpression, SingleExpression};
use jswt_common::Type;
use jswt_symbols::BindingsTable;

#[derive(Debug)]
pub struct MirNewLoweringContext<'a> {
    bindings: &'a BindingsTable,
}

impl<'a> MirNewLoweringContext<'a> {
    pub fn new(bindings: &'a BindingsTable) -> Self {
        Self { bindings }
    }
}

impl<'a> TransformVisitor for MirNewLoweringContext<'a> {
    fn visit_new(&mut self, node: &NewExpression) -> SingleExpression {
        // New expressions are desugared to a call to a constructor function.
        // visit the arguments to ensure that they are lowered if necessary
        let node_as_args = node.expression.as_arguments().expect(&format!(
            "New expressions should be followed by an arguments call"
        ));

        let args_exp = self.visit_argument_expression(node_as_args);
        let mut args_exp = args_exp.as_arguments().unwrap().clone();

        // Change the target of the arguments call to the constructor function
        let ident_exp = args_exp.ident.as_identifier_mut().unwrap();
        let ident_name = ident_exp.ident.value.clone();

        // find the class binding for the identifier
        let _ = self.bindings.lookup(&ident_name).expect(&format!(
            "Could not find class binding for identifier {}",
            ident_name
        ));

        ident_exp.ident.value = format!("{}#constructor", ident_name).into();
        args_exp.ty = Type::Binding(ident_name);
        SingleExpression::Arguments(args_exp)
    }
}

#[cfg(test)]
mod test {

    use jswt_assert::assert_debug_snapshot;
    use jswt_parser::Parser;
    use jswt_semantics::GlobalSemanticResolver;
    use jswt_symbols::ScopedSymbolTable;
    use jswt_tokenizer::Tokenizer;

    use super::*;
    use crate::MirLoweringContext;

    #[test]
    fn test_new_lowering_lowers_new_expression_into_constructor_invocation() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_class_declaration_lowers_new_expression_into_constructor_invocation",
            r"
        class Array {
            constructor() {}
        }

        function main(): i32 {
            let x = new Array();
            return 0;
        }
    ",
        );

        let ast = Parser::new(&mut tokenizer).parse();

        let mut symbol_table = ScopedSymbolTable::default();
        let mut bindings_table = BindingsTable::default();
        let mut global_resolver =
            GlobalSemanticResolver::new(&mut bindings_table, &mut symbol_table);
        global_resolver.resolve(&ast);

        // No errors in global resolver
        assert!(global_resolver.errors().len() == 0);

        let mut lowering = MirLoweringContext::new(&bindings_table, &symbol_table);
        let lowered = lowering.lower(&ast);
        assert_debug_snapshot!(lowered);
    }
}
