use jswt_ast::{
    transform::{walk_argument_expression, TransformVisitor},
    ArgumentsExpression, SingleExpression,
};
use jswt_common::Typeable;
use jswt_symbols::BindingsTable;
use jswt_synthetic::function_call;

pub struct MirArgumentsLoweringContext<'a> {
    bindings: &'a BindingsTable,
}

impl<'a> MirArgumentsLoweringContext<'a> {
    pub fn new(bindings: &'a BindingsTable) -> Self {
        Self { bindings }
    }
}

impl TransformVisitor for MirArgumentsLoweringContext<'_> {
    fn visit_argument_expression(&mut self, node: &ArgumentsExpression) -> SingleExpression {
        if let SingleExpression::MemberDot(dot) = &*node.ident {
            // We want to rewrite function calls like `foo.bar(baz)` into
            // ty#bar(foo, baz)

            let lhs = self.visit_single_expression(&dot.target);
            let target_type = dot.target.ty();
            let type_name = target_type.to_string();

            let ident_exp = dot.expression.as_identifier().unwrap();

            let ident_name = ident_exp.ident.value.clone();

            // format as type#function(args)
            let function_name = format!("{}#{}", target_type.to_string(), ident_name).into();

            // lhs is the first param 
            let mut args = vec![lhs];
            for arg in &node.arguments.arguments {
                args.push(self.visit_single_expression(&arg));
            }

            // get the return type of the method from the bindings table
            println!("Looking up method {} in class {}", ident_name, type_name);
            let class_binding = self.bindings.lookup(&type_name).unwrap();
            let method_binding = class_binding.method(&ident_name).unwrap();

            let f = function_call(
                function_name,
                args,
                method_binding.signature.returns.to_owned(),
            );

            return f;
            // let args = node.arguments;

            // let node_as_args = node.expression.as_arguments().expect(&format!(
            //     "New expressions should be followed by an arguments call"
            // ));

            // let args_exp = self.visit_argument_expression(node_as_args);
            // let mut args_exp = args_exp.as_arguments().unwrap().clone();

            // // Change the target of the arguments call to the constructor function
            // let ident_exp = args_exp.ident.as_identifier_mut().unwrap();
            // let ident_name = ident_exp.ident.value.clone();

            // // find the class binding for the identifier
            // let _ = self.bindings.lookup(&ident_name).expect(&format!(
            //     "Could not find class binding for identifier {}",
            //     ident_name
            // ));

            // ident_exp.ident.value = format!("{}#constructor", ident_name).into();
            // args_exp.ty = Type::Binding(ident_name);
            // SingleExpression::Arguments(args_exp)
        }

        walk_argument_expression(self, node)
    }
}
