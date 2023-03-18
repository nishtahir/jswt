use super::GlobalSemanticResolver;
use crate::SemanticError;
use jswt_ast::{visit::Visitor, FunctionDeclarationElement};
use jswt_common::Type;
use jswt_symbols::{ScopedSymbolTable, Symbol};

pub struct FunctionDeclarationGlobalContext<'a> {
    symbols: &'a mut ScopedSymbolTable,
    errors: &'a mut Vec<SemanticError>,
}

impl<'a> FunctionDeclarationGlobalContext<'a> {
    pub fn new(resolver: &'a mut GlobalSemanticResolver) -> Self {
        Self {
            symbols: resolver.symbols,
            errors: &mut resolver.errors,
        }
    }
}

impl<'a> Visitor for FunctionDeclarationGlobalContext<'a> {
    fn visit_function_declaration(&mut self, node: &FunctionDeclarationElement) {
        let ident = &node.ident;
        let function_name = &ident.value;

        if self.symbols.lookup_current(function_name).is_some() {
            let error = SemanticError::FunctionAlreadyDefined {
                name: function_name.clone(),
                span: ident.span.to_owned(),
            };
            self.errors.push(error);
        }

        // Determine the return type of the function
        let returns = node
            .returns
            .as_ref()
            .map(|it| it.ty.clone())
            .unwrap_or(Type::Binding("void".into()));

        // Determine the types of the function parameters
        let params = node
            .params
            .parameters
            .iter()
            .map(|param| param.type_annotation.ty.clone())
            .collect();

        // TODO - we're eventually going to want to qualify the full function
        // name here scoped to the current module, but for now we'll just
        // use the function name
        self.symbols
            .define(function_name, Symbol::function(params, returns));
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use jswt_assert::assert_debug_snapshot;
    use jswt_parser::Parser;
    use jswt_symbols::BindingsTable;
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
        let mut symbols = ScopedSymbolTable::default();
        let mut bindings = BindingsTable::default();
        let mut resolver = GlobalSemanticResolver::new(&mut bindings, &mut symbols);
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
        let mut symbols = ScopedSymbolTable::default();
        let mut bindings = BindingsTable::default();
        let mut resolver = GlobalSemanticResolver::new(&mut bindings, &mut symbols);
        resolver.resolve(&ast);

        assert_debug_snapshot!(resolver);
    }
}
