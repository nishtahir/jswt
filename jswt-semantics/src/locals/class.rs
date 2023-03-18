use super::LocalSemanticResolver;
use crate::{SemanticError, SymbolTable};
use jswt_ast::{
    visit::{self, Visitor},
    ClassDeclarationElement,
};
use jswt_common::Spannable;
use jswt_symbols::Symbol;

pub struct ClassLocalContext<'a> {
    symbols: &'a mut SymbolTable,
    errors: &'a mut Vec<SemanticError>,
}

impl<'a> ClassLocalContext<'a> {
    pub fn new(resolver: &'a mut LocalSemanticResolver, class: &ClassDeclarationElement) -> Self {
        Self {
            symbols: resolver.symbols,
            errors: &mut resolver.errors,
        }
    }
}

impl<'a> Visitor for ClassLocalContext<'a> {
    fn visit_class_body(&mut self, node: &jswt_ast::ClassBody) {
        self.symbols.push_scope();
        visit::walk_class_body(self, node);
        self.symbols.pop_scope();
    }

    fn visit_class_method_declaration(&mut self, node: &jswt_ast::ClassMethodElement) {
        self.symbols.push_scope();
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
            self.symbols.define(
                param_name.clone(),
                Symbol::ty(param.type_annotation.ty.clone()),
            );
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
    fn test_class_resolved_variables_in_method() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_class_resolved_variables_in_method",
            r"
        class Test { 
            method(a: i32, b: i32) {
                let c = 1;
                let d = 2;
            }
        }
        ",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut symbols = SymbolTable::default();
        let mut bindings = BindingsTable::default();

        let mut global = GlobalSemanticResolver::new(&mut bindings, &mut symbols);
        global.resolve(&ast);
        let mut local = LocalSemanticResolver::new(&mut bindings, &mut symbols);
        local.resolve(&ast);

        assert_debug_snapshot!(local);
    }
}
