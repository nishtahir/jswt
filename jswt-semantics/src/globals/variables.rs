use jswt_ast::{visit::Visitor, AssignableElement};
use jswt_common::Typeable;
use jswt_symbols::{Symbol, TypeSignature};

use crate::{SemanticError, SymbolTable};

use super::GlobalSemanticResolver;

pub struct VariableDeclarationGlobalContext<'a> {
    symbols: &'a mut SymbolTable,
    errors: &'a mut Vec<SemanticError>,
}

impl<'a> VariableDeclarationGlobalContext<'a> {
    pub fn new(resolver: &'a mut GlobalSemanticResolver) -> Self {
        Self {
            symbols: resolver.symbols,
            errors: &mut resolver.errors,
        }
    }
}

impl<'a> Visitor for VariableDeclarationGlobalContext<'a> {
    fn visit_variable_statement(&mut self, node: &jswt_ast::VariableStatement) {
        match &node.target {
            AssignableElement::Identifier(ident) => {
                let name = &ident.value;
                // If a variable with the same name already exists 
                // in the current scope then we have a duplicate variable error
                if self.symbols.lookup_current(name).is_some() {
                    let error = SemanticError::VariableAlreadyDefined {
                        name: name.clone(),
                        span: ident.span.to_owned(),
                    };
                    self.errors.push(error);
                }

                // Add the variable to the symbol table
                // The type of the symbol is the type of the rhs expression
                self.symbols.define(
                    name.clone(),
                    Symbol::Type(TypeSignature {
                        ty: node.expression.ty(),
                    }),
                );
            }
        }
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
    fn test_context_resolves_global_variables() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_context_resolves_global_variables",
            r"
        const PI = 3.14;

        function test(a: i32, b: i32) {
            let x = 99;
        }
        ",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut symbols = SymbolTable::default();
        let mut bindings = BindingsTable::default();
        let mut resolver = GlobalSemanticResolver::new(&mut bindings, &mut symbols);
        resolver.resolve(&ast);

        assert_debug_snapshot!(resolver);
    }

    #[test]
    fn test_context_resolves_duplicate_variable_error() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_context_resolves_duplicate_variable_error",
            r"
        const PI = 3.14;
        const PI = 3.15;

        function test(a: i32, b: i32) {
            let x = 99;
        }
        ",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut symbols = SymbolTable::default();
        let mut bindings = BindingsTable::default();
        let mut resolver = GlobalSemanticResolver::new(&mut bindings, &mut symbols);
        resolver.resolve(&ast);

        assert_debug_snapshot!(resolver);
    }
}
