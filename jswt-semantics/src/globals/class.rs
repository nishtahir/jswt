use super::GlobalSemanticResolver;
use crate::SemanticError;
use jswt_ast::{
    visit::{self, Visitor},
    ClassDeclarationElement,
};
use jswt_common::Type;
use jswt_symbols::{BindingsTable, ClassBinding, FunctionSignature, Method};

pub struct ClassDeclarationGlobalContext<'a> {
    class_binding: ClassBinding,
    bindings: &'a mut BindingsTable,
    errors: &'a mut Vec<SemanticError>,
}

impl<'a> ClassDeclarationGlobalContext<'a> {
    /// We need the class reference here to
    /// figure out the class name
    pub fn new(resolver: &'a mut GlobalSemanticResolver, node: &ClassDeclarationElement) -> Self {
        Self {
            class_binding: ClassBinding {
                name: node.ident.value.clone(),
                fields: vec![],
                methods: vec![],
            },
            bindings: resolver.bindings,
            errors: &mut resolver.errors,
        }
    }
}

impl<'a> Visitor for ClassDeclarationGlobalContext<'a> {
    fn visit_class_declaration(&mut self, node: &ClassDeclarationElement) {
        let class_name = node.ident.value.clone();
        // Walk the rest of the class tree
        // Resolve fields, and methods
        visit::walk_class_declaration(self, &node);

        // Add the class binding to the bindings table
        self.bindings
            .insert(class_name, self.class_binding.to_owned());
    }

    fn visit_class_field_declaration(&mut self, node: &jswt_ast::ClassFieldElement) {
        let field_name = node.ident.value.clone();

        // Check if the field already exists
        if self
            .class_binding
            .fields
            .iter()
            .any(|f| f.name == field_name)
        {
            let error = SemanticError::FieldAlreadyDefined {
                name: field_name.clone(),
                span: node.ident.span.to_owned(),
            };
            self.errors.push(error);
        }

        self.class_binding.fields.push(jswt_symbols::Field {
            name: field_name,
            // Fields are aligned in the order they are declared
            index: self.class_binding.fields.len(),
            // We need to get the type of the field
            // from the symbol table to compute the size
            size: 4, // TODO compute type size
            ty: node.type_annotation.ty.clone(),
        });
        visit::walk_class_field_declaration(self, node);
    }

    fn visit_class_method_declaration(&mut self, node: &jswt_ast::ClassMethodElement) {
        let method_name = node.ident.value.clone();

        // Check if the method already exists
        if self
            .class_binding
            .methods
            .iter()
            .any(|m| m.name == method_name)
        {
            let error = SemanticError::MethodAlreadyDefined {
                name: method_name.clone(),
                span: node.ident.span.to_owned(),
            };
            self.errors.push(error);
        }

        // Get the parameters and return type
        let params = node
            .params
            .parameters
            .iter()
            .map(|param| param.type_annotation.ty.clone())
            .collect();
        let returns = node
            .returns
            .as_ref()
            .map(|ty| ty.ty.clone())
            .unwrap_or(Type::Binding("void".into()));

        // Add the method to the class binding
        self.class_binding.methods.push(Method {
            name: method_name,
            signature: FunctionSignature { params, returns },
        });
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
    fn test_context_resolves_class() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_context_resolves_class",
            r"
            class Foo {
                bar: i32;
                baz(a: i32): i32 {
                    return 0;
                }
                empty() {}
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
    fn test_context_reports_field_already_defined_error() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_context_reports_field_already_defined_error",
            r"
            class Foo {
                bar: i32;
                bar: i32;
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
