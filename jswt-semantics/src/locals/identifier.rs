use super::LocalSemanticResolver;
use crate::SemanticError;
use jswt_ast::visit::Visitor;
use jswt_common::Spannable;
use jswt_symbols::ScopedSymbolTable;

pub struct IdentifierExpressionLocalContext<'a> {
    symbols: &'a mut ScopedSymbolTable,
    errors: &'a mut Vec<SemanticError>,
}

impl<'a> IdentifierExpressionLocalContext<'a> {
    pub fn new(resolver: &'a mut LocalSemanticResolver) -> Self {
        Self {
            symbols: resolver.symbols,
            errors: &mut resolver.errors,
        }
    }
}

impl<'a> Visitor for IdentifierExpressionLocalContext<'a> {
    fn visit_identifier_expression(&mut self, node: &jswt_ast::IdentifierExpression) {
        let ident = &node.ident;
        let name = &ident.value;
        let symbol = self.symbols.lookup(name);
        if let None = symbol {
            let error = SemanticError::VariableNotDefined {
                name: name.clone(),
                span: ident.span(),
            };
            self.errors.push(error);
        };
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
}
