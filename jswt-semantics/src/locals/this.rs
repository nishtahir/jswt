use jswt_ast::{visit::Visitor, ThisExpression};
use jswt_common::Spannable;

use crate::{LocalSemanticResolver, SemanticError};

pub struct ThisExpressionLocalContext<'a> {
    errors: &'a mut Vec<SemanticError>,
}

impl<'a> ThisExpressionLocalContext<'a> {
    pub fn new(resolver: &'a mut LocalSemanticResolver) -> Self {
        Self {
            errors: &mut resolver.errors,
        }
    }
}

impl<'a> Visitor for ThisExpressionLocalContext<'a> {
    fn visit_this_expression(&mut self, node: &ThisExpression) {
        // Here we assume that we're not in class declaration context.
        // That's handled by class::ClassDeclarationGlobalContext. So
        // we can just emit an error here.
        self.errors
            .push(SemanticError::ThisOutsideClass { span: node.span() })
    }
}

#[cfg(test)]
mod test {
    use crate::GlobalSemanticResolver;

    use super::*;
    use jswt_assert::assert_debug_snapshot;
    use jswt_parser::Parser;
    use jswt_symbols::{BindingsTable, ScopedSymbolTable};
    use jswt_tokenizer::Tokenizer;

    #[test]
    fn test_context_errors_on_this_outside_class() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_context_errors_on_this_outside_class",
            r"
            function foo() {
                this;
            }
        ",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut symbols = ScopedSymbolTable::default();
        let mut bindings = BindingsTable::default();
        let mut global = GlobalSemanticResolver::new(&mut bindings, &mut symbols);
        global.resolve(&ast);
        let mut resolver = LocalSemanticResolver::new(&mut bindings, &mut symbols);
        resolver.resolve(&ast);

        assert_debug_snapshot!(resolver);
    }
}
