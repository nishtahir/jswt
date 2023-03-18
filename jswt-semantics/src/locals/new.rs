use super::LocalSemanticResolver;
use crate::SemanticError;
use jswt_ast::{visit::Visitor, NewExpression};
use jswt_common::Spannable;
use jswt_symbols::{BindingsTable, ScopedSymbolTable};

pub struct NewExpressionLocalContext<'a> {
    bindings: &'a BindingsTable,
    symbols: &'a mut ScopedSymbolTable,
    errors: &'a mut Vec<SemanticError>,
}

impl<'a> NewExpressionLocalContext<'a> {
    pub fn new(resolver: &'a mut LocalSemanticResolver) -> Self {
        Self {
            bindings: resolver.bindings,
            symbols: resolver.symbols,
            errors: &mut resolver.errors,
        }
    }
}

impl<'a> Visitor for NewExpressionLocalContext<'a> {
    fn visit_new(&mut self, node: &NewExpression) {
        // This should always be an arguments expression
        // TODO - Update the AST to make this more clear
        let arguments_exp = node.expression.as_arguments().unwrap();
        let ident_exp = arguments_exp.ident.as_identifier().unwrap();
        let ident = &ident_exp.ident.value;

        if let None = self.bindings.lookup(ident) {
            self.errors.push(SemanticError::ClassNotDefined {
                ident: ident.clone(),
                span: ident_exp.span(),
            });
        }
        // Check to see if the class is defined in the bindings table
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

    #[test]
    fn test_error_on_undefined_class() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_error_on_undefined_class",
            r"
        function test() {
            new Test();
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
