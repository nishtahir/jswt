use jswt_ast::{visit::Visitor, ThisExpression};
use jswt_common::Spannable;

use crate::{GlobalSemanticResolver, SemanticError};

pub struct ThisExpressionGlobalContext<'a> {
    errors: &'a mut Vec<SemanticError>,
}

impl<'a> ThisExpressionGlobalContext<'a> {
    pub fn new(resolver: &'a mut GlobalSemanticResolver) -> Self {
        Self {
            errors: &mut resolver.errors,
        }
    }
}

impl<'a> Visitor for ThisExpressionGlobalContext<'a> {
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
    use crate::SymbolTable;

    use super::*;
    use jswt_assert::assert_debug_snapshot;
    use jswt_parser::Parser;
    use jswt_symbols::BindingsTable;
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
        let mut symbols = SymbolTable::default();
        let mut bindings = BindingsTable::default();
        let mut resolver = GlobalSemanticResolver::new(&mut bindings, &mut symbols);
        resolver.resolve(&ast);

        assert_debug_snapshot!(resolver);
    }
}
