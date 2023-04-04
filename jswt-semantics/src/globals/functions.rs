use super::GlobalSemanticResolver;
use crate::SemanticError;
use jswt_ast::{visit::Visitor, FunctionDeclarationElement};
use jswt_common::Type;
use jswt_symbols::{Function, Parameter, SemanticEnvironment, Symbol, SymbolTable};

pub struct FunctionDeclarationGlobalContext<'a> {
    environment: &'a mut SemanticEnvironment,
    errors: &'a mut Vec<SemanticError>,
}

impl<'a> FunctionDeclarationGlobalContext<'a> {
    pub fn new(resolver: &'a mut GlobalSemanticResolver) -> Self {
        Self {
            environment: resolver.environment,
            errors: &mut resolver.errors,
        }
    }
}

impl<'a> Visitor for FunctionDeclarationGlobalContext<'a> {
    fn visit_function_declaration(&mut self, node: &FunctionDeclarationElement) {
        let ident = &node.ident;
        let function_name = &ident.value;

        if self.environment.get_symbol(function_name).is_some() {
            let error = SemanticError::FunctionAlreadyDefined {
                name: function_name.clone(),
                span: ident.span.to_owned(),
            };
            self.errors.push(error);
        }

        // Determine the return type of the function
        // TODO - check that the return type is valid
        let returns = node
            .returns
            .as_ref()
            .map(|it| it.ty.clone())
            .unwrap_or(Type::Binding("void".into()));

        let mut params = vec![];
        for param in &node.params.parameters {
            // Check if the parameter already exists
            if params
                .iter()
                .any(|p: &Parameter| p.name == param.ident.value)
            {
                // TODO - rename this to parameter already defined
                let error = SemanticError::VariableAlreadyDefined {
                    name: param.ident.value.clone(),
                    span: param.ident.span.to_owned(),
                };
                self.errors.push(error);
            }
            params.push(Parameter {
                name: param.ident.value.clone(),
                ty: param.type_annotation.ty.clone(),
            });
        }

        // TODO - we're eventually going to want to qualify the full function
        // name here scoped to the current module, but for now we'll just
        // use the function name
        self.environment.insert_symbol(
            function_name,
            Symbol::Function(Function {
                name: function_name.clone(),
                params,
                ret: returns,
            }),
        );
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use jswt_assert::assert_debug_snapshot;
    use jswt_parser::Parser;
    use jswt_tokenizer::Tokenizer;

    #[test]
    fn test_context_resolves_function_bindings() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_context_resolves_function_bindings",
            r"
        function test(a: i32, b: i32) {
            let x = 99;
        }
 
        function test2(): Array {

        }
        ",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut environment = SemanticEnvironment::default();
        let mut resolver = GlobalSemanticResolver::new(&mut environment);
        resolver.resolve(&ast);

        assert_debug_snapshot!(resolver);
    }

    #[test]
    fn test_context_reports_duplicate_function_error() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_context_reports_duplicate_function_error",
            r"
        function test2(): Array {

        }
        function test2(): Array {

        }
        ",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut environment = SemanticEnvironment::default();
        let mut resolver = GlobalSemanticResolver::new(&mut environment);
        resolver.resolve(&ast);

        assert_debug_snapshot!(resolver);
    }
}
