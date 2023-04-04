use jswt_ast::{transform::TransformVisitor, NewExpression, SingleExpression};

use jswt_symbols::{BindingsTable, SemanticEnvironment};

#[derive(Debug)]
pub struct MirNewLoweringContext<'a> {
    environment: &'a SemanticEnvironment,
}

impl<'a> MirNewLoweringContext<'a> {
    pub fn new(environment: &'a SemanticEnvironment) -> Self {
        Self { environment }
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
        let _ = self.environment.get_binding(&ident_name).expect(&format!(
            "Could not find class binding for identifier {}",
            ident_name
        ));

        ident_exp.ident.value = format!("{}#constructor", ident_name).into();
        SingleExpression::Arguments(args_exp)
    }
}

#[cfg(test)]
mod test {

    use jswt_assert::assert_debug_snapshot;
    use jswt_parser::Parser;
    use jswt_semantics::{GlobalSemanticResolver, LocalSemanticResolver};
    
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

        let mut environment = SemanticEnvironment::default();

        let mut global = GlobalSemanticResolver::new(&mut environment);
        global.resolve(&ast);
        assert!(global.errors().is_empty(), "{:?}", global.errors());

        let mut local = LocalSemanticResolver::new(&mut environment);
        local.resolve(&ast);
        assert!(local.errors().is_empty(), "{:?}", local.errors());

        // No errors in global resolver

        let mut lowering = MirLoweringContext::new(&environment);
        let lowered = lowering.lower(&ast);
        assert_debug_snapshot!(lowered);
    }
}
