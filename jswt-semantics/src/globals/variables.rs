use crate::SemanticError;
use jswt_ast::*;
use jswt_common::{Spannable, Type};
use jswt_symbols::{SemanticEnvironment, Symbol, SymbolTable, Variable};

use super::GlobalSemanticResolver;

pub struct VariableDeclarationGlobalContext<'a> {
    environment: &'a mut SemanticEnvironment,
    errors: &'a mut Vec<SemanticError>,
}

impl<'a> VariableDeclarationGlobalContext<'a> {
    pub fn new(resolver: &'a mut GlobalSemanticResolver) -> Self {
        Self {
            environment: resolver.environment,
            errors: &mut resolver.errors,
        }
    }
}

impl<'a> Visitor for VariableDeclarationGlobalContext<'a> {
    fn visit_variable_declaration_element(&mut self, node: &VariableDeclarationElement) {
        let name = &node.name.value;
        // If a variable with the same name already exists
        // in the current scope then we have a duplicate variable error
        if let Some(_) = self.environment.get_symbol(name) {
            let error = SemanticError::VariableAlreadyDefined {
                name: name.clone(),
                span: node.name.span(),
            };
            self.errors.push(error);
        }

        let is_mutable = match node.modifier {
            VariableModifier::Let(_) => true,
            VariableModifier::Const(_) => false,
        };

        // Add the variable to the symbol table
        // The type of the symbol is the type of the rhs expression
        // We only care about the declaration of the variable at this stage
        // The initialization of the variable is handled by the local resolver
        self.environment.insert_symbol(
            name,
            Symbol::Variable(Variable {
                ty: Type::Unknown,
                declaration: node.span(),
                initialization: None,
                name: name.clone(),
                is_mutable,
            }),
        );
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use jswt_assert::assert_debug_snapshot;
    use jswt_parser::Parser;
    use jswt_symbols::SemanticEnvironment;
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
        let mut environment = SemanticEnvironment::default();
        let mut resolver = GlobalSemanticResolver::new(&mut environment);
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
        let mut environment = SemanticEnvironment::default();
        let mut resolver = GlobalSemanticResolver::new(&mut environment);
        resolver.resolve(&ast);

        assert_debug_snapshot!(resolver);
    }
}
