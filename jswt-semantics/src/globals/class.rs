use super::GlobalSemanticResolver;
use crate::SemanticError;
use jswt_ast::*;
use jswt_common::{Spannable, Type};
use jswt_symbols::{
    BindingsTable, ClassBinding, Constructor, Field, Method, Parameter, SemanticEnvironment,
    Symbol, SymbolTable,
};

pub struct ClassDeclarationGlobalContext<'a> {
    class_binding: ClassBinding,
    environment: &'a mut SemanticEnvironment,
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
                constructors: vec![],
            },
            environment: resolver.environment,
            errors: &mut resolver.errors,
        }
    }
}

impl<'a> Visitor for ClassDeclarationGlobalContext<'a> {
    fn visit_class_declaration_element(&mut self, node: &ClassDeclarationElement) {
        let class_name = node.ident.value.clone();
        // Walk the rest of the class tree
        // Resolve fields, and methods
        walk_class_declaration_element(self, &node);

        // Add the class binding to the bindings table
        self.environment
            .insert_binding(class_name.clone(), self.class_binding.to_owned());

        // Add the class to the symbol table
        self.environment.insert_symbol(&class_name, Symbol::Class);
    }

    fn visit_class_constructor_element(&mut self, node: &ClassConstructorElement) {
        // TODO - Check if the constructor already exists
        // Get the parameters
        let mut params = vec![];
        for param in node.params.parameters.iter() {
            // Check if the parameter already exists
            if params
                .iter()
                .any(|p: &Parameter| p.name == param.ident.value)
            {
                let error = SemanticError::VariableAlreadyDefined {
                    name: param.ident.value.clone(),
                    span: param.span(),
                };
                self.errors.push(error);
            }

            params.push(Parameter {
                name: param.ident.value.clone(),
                ty: param.type_annotation.ty.clone(),
            });
        }
        // Add the constructor to the class binding
        self.class_binding.constructors.push(Constructor { params });
        walk_class_constructor_element(self, node);
    }

    fn visit_class_field_element(&mut self, node: &ClassFieldElement) {
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

        let field_type = node.type_annotation.ty.clone();
        self.class_binding.fields.push(Field {
            name: field_name,
            // Fields are aligned in the order they are declared
            index: self.class_binding.fields.len(),
            // We need to get the type of the field
            // from the symbol table to compute the size
            size: 4, // TODO compute type size
            ty: field_type,
        });
        walk_class_field_element(self, node);
    }

    fn visit_class_method_element(&mut self, node: &ClassMethodElement) {
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

        let mut params = vec![];
        for param in &node.params.parameters {
            // Check if the parameter already exists
            if params
                .iter()
                .any(|p: &Parameter| p.name == param.ident.value)
            {
                // TODO - rename this to parameter already defined
                let error = SemanticError::VariableAlreadyDefined {
                    name: param.ident.value.clone(),
                    span: param.ident.span.to_owned(),
                };
                self.errors.push(error);
            }

            params.push(Parameter {
                name: param.ident.value.clone(),
                ty: param.type_annotation.ty.clone(),
            });
        }

        // TODO - check that the return type is valid
        let returns = node
            .returns
            .as_ref()
            .map(|ty| ty.ty.clone())
            .unwrap_or(Type::Binding("void".into()));

        // Add the method to the class binding
        self.class_binding.methods.push(Method {
            name: method_name,
            params,
            ret: returns,
        });
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
        let mut environment = SemanticEnvironment::default();
        let mut resolver = GlobalSemanticResolver::new(&mut environment);
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
        let mut environment = SemanticEnvironment::default();
        let mut resolver = GlobalSemanticResolver::new(&mut environment);
        resolver.resolve(&ast);

        assert_debug_snapshot!(resolver);
    }
}
