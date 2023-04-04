use super::LocalSemanticResolver;
use crate::SemanticError;
use jswt_ast::{visit::Visitor, NewExpression, SingleExpression};
use jswt_common::{Spannable, Type};
use jswt_symbols::{SemanticEnvironment, Symbol, SymbolTable, TypesTable};

pub struct NewExpressionLocalContext<'a> {
    environment: &'a mut SemanticEnvironment,
    errors: &'a mut Vec<SemanticError>,
}

impl<'a> NewExpressionLocalContext<'a> {
    pub fn new(resolver: &'a mut LocalSemanticResolver) -> Self {
        Self {
            environment: resolver.environment,
            errors: &mut resolver.errors,
        }
    }
}

impl<'a> Visitor for NewExpressionLocalContext<'a> {
    fn visit_new(&mut self, node: &NewExpression) {
        // The inner expression should be an arguments expression
        
    }
}

#[cfg(test)]
mod test {

    use crate::GlobalSemanticResolver;

    use super::*;
    use jswt_assert::assert_debug_snapshot;
    use jswt_parser::Parser;
    use jswt_symbols::SemanticEnvironment;
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
        let mut environment = SemanticEnvironment::default();

        let mut global = GlobalSemanticResolver::new(&mut environment);
        global.resolve(&ast);
        let mut local = LocalSemanticResolver::new(&mut environment);
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
        let mut environment = SemanticEnvironment::default();

        let mut global = GlobalSemanticResolver::new(&mut environment);
        global.resolve(&ast);
        let mut local = LocalSemanticResolver::new(&mut environment);
        local.resolve(&ast);

        assert_debug_snapshot!(local);
    }
}
