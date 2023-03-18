use super::LocalSemanticResolver;
use crate::SemanticError;
use jswt_ast::{
    visit::{self, Visitor},
    AssignableElement, VariableStatement,
};
use jswt_common::Spannable;
use jswt_symbols::{ScopedSymbolTable, Symbol};

pub struct VariableDeclarationLocalContext<'a> {
    symbols: &'a mut ScopedSymbolTable,
    errors: &'a mut Vec<SemanticError>,
}

impl<'a> VariableDeclarationLocalContext<'a> {
    pub fn new(resolver: &'a mut LocalSemanticResolver) -> Self {
        Self {
            symbols: resolver.symbols,
            errors: &mut resolver.errors,
        }
    }
}

impl<'a> Visitor for VariableDeclarationLocalContext<'a> {
    fn visit_variable_statement(&mut self, node: &VariableStatement) {
        // Check if we're in a local scope
        if self.symbols.depth() > 1 {
            let name = match &node.target {
                AssignableElement::Identifier(ident) => &ident.value,
            };

            // Error if a variable name collides with an already defined variable
            if self.symbols.lookup(name).is_some() {
                let error = SemanticError::VariableAlreadyDefined {
                    name: name.clone(),
                    span: node.target.span(),
                };
                self.errors.push(error);
            }

            // Figure out the type information if we can
            let declared_type = node
                .type_annotation
                .as_ref()
                .map(|t| Symbol::ty(t.ty.clone()))
                .unwrap_or(Symbol::Unknown);

            self.symbols.define(name, declared_type);
        }
        visit::walk_variable_statement(self, node);
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
    fn test_local_context_resolves_local_variables() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_local_context_resolves_local_variables",
            r"
        const PI = 3.14;

        function test(a: i32, b: i32) {
            let x = 99;
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
    #[ignore = "Doesn't work yet"]
    fn test_local_context_resolves_duplicate_variable_error() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_local_context_resolves_duplicate_variable_error",
            r"
        function test(a: i32, b: i32) {
            let x = 99;
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
