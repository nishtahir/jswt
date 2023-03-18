use super::LocalSemanticResolver;
use crate::SemanticError;
use jswt_ast::{visit::Visitor, FunctionDeclarationElement};
use jswt_common::Spannable;
use jswt_symbols::{ScopedSymbolTable, Symbol};

pub struct FunctionsLocalContext<'a> {
    symbols: &'a mut ScopedSymbolTable,
    errors: &'a mut Vec<SemanticError>,
}

impl<'a> FunctionsLocalContext<'a> {
    pub fn new(resolver: &'a mut LocalSemanticResolver) -> Self {
        Self {
            symbols: resolver.symbols,
            errors: &mut resolver.errors,
        }
    }
}

impl<'a> Visitor for FunctionsLocalContext<'a> {
    fn visit_function_declaration(&mut self, node: &FunctionDeclarationElement) {
        self.symbols.push_scope(node.body.span());
        // Redefinition errors are handled by the GlobalSemanticResolver
        // during clobal symbol resolution

        //  Add function parameters as variables in scope
        for param in node.params.parameters.iter() {
            // Check to see if the parameter is already defined
            let param_name = &param.ident.value;
            if let Some(_) = self.symbols.lookup(param_name) {
                let error = SemanticError::VariableAlreadyDefined {
                    name: param_name.clone(),
                    span: param.span(),
                };
                self.errors.push(error);
            }

            // Resolve Type from Type Annotation
            self.symbols
                .define(param_name, Symbol::ty(param.type_annotation.ty.clone()));
        }

        self.symbols.pop_scope();
    }
}

#[cfg(test)]
mod test {

    use crate::GlobalSemanticResolver;

    use super::*;
    use jswt_assert::assert_debug_snapshot;
    use jswt_parser::Parser;
    use jswt_symbols::BindingsTable;
    use jswt_tokenizer::Tokenizer;

    #[test]
    fn test_function_parameters_are_in_scope() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_function_parameters_are_in_scope",
            r"
        function test(x: number, y: number) {
            x;
        }
        ",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut symbols = ScopedSymbolTable::default();
        let mut bindings = BindingsTable::default();

        let mut global = GlobalSemanticResolver::new(&mut bindings, &mut symbols);
        global.resolve(&ast);
        let mut local = LocalSemanticResolver::new(&mut bindings, &mut symbols);
        local.resolve(&ast);

        assert_debug_snapshot!(local);
    }

    #[test]
    fn test_error_on_undefined_variable() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_error_on_undefined_variable",
            r"
        function test() {
            x;
        }
        ",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut symbols = ScopedSymbolTable::default();
        let mut bindings = BindingsTable::default();

        let mut global = GlobalSemanticResolver::new(&mut bindings, &mut symbols);
        global.resolve(&ast);
        let mut local = LocalSemanticResolver::new(&mut bindings, &mut symbols);
        local.resolve(&ast);

        assert_debug_snapshot!(local);
    }

    #[test]
    fn test_function_parameter_redefinition() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_function_parameter_redefinition",
            r"
        function test(x: number, x: number) {
            x;
        }
        ",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut symbols = ScopedSymbolTable::default();
        let mut bindings = BindingsTable::default();

        let mut global = GlobalSemanticResolver::new(&mut bindings, &mut symbols);
        global.resolve(&ast);
        let mut local = LocalSemanticResolver::new(&mut bindings, &mut symbols);
        local.resolve(&ast);

        assert_debug_snapshot!(local);
    }
}
