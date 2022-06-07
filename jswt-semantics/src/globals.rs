use crate::{SemanticError, SymbolTable};

use jswt_ast::{visit::*, *};
use jswt_common::Type;
use jswt_symbols::{BindingsTable, ClassBinding, Field, FunctionSignature, Method, Symbol};

#[derive(Debug)]
pub struct GlobalResolver<'a> {
    bindings_table: &'a mut BindingsTable,
    symbol_table: &'a mut SymbolTable,
    bindings_context: Vec<ClassBinding>,
    pub errors: Vec<SemanticError>,
}

impl<'a> GlobalResolver<'a> {
    pub fn new(bindings_table: &'a mut BindingsTable, symbol_table: &'a mut SymbolTable) -> Self {
        Self {
            bindings_table,
            symbol_table,
            bindings_context: vec![],
            errors: vec![],
        }
    }

    pub(crate) fn resolve(&mut self, ast: &Ast) {
        self.visit_program(&ast.program);
        debug_assert!(self.symbol_table.depth() == 1);
    }
}

impl<'a> Visitor for GlobalResolver<'a> {
    fn visit_program(&mut self, node: &Program) {
        walk_program(self, node);
    }

    fn visit_variable_statement(&mut self, node: &VariableStatement) {
        match &node.target {
            AssignableElement::Identifier(ident) => {
                let name = &ident.value;
                if self.symbol_table.lookup_current(name).is_some() {
                    let error = SemanticError::VariableAlreadyDefined {
                        name: name.clone(),
                        span: ident.span.to_owned(),
                    };
                    self.errors.push(error);
                }
                self.symbol_table.define(name.clone(), Symbol::Unknown);
            }
        }
    }

    fn visit_function_declaration(&mut self, node: &FunctionDeclarationElement) {
        // add to global symbol table
        let ident = &node.ident;
        let name = &ident.value;

        let returns = node
            .returns
            .as_ref()
            .map(|it| it.ty.clone())
            .unwrap_or(Type::Binding("void".into()));

        let params = node
            .params
            .parameters
            .iter()
            .map(|param| param.type_annotation.ty.clone())
            .collect();

        if self.symbol_table.lookup_current(name).is_some() {
            let error = SemanticError::FunctionAlreadyDefined {
                name: name.clone(),
                span: ident.span.to_owned(),
            };
            self.errors.push(error);
        }

        self.symbol_table
            .define(name.clone(), Symbol::function(params, returns));
    }

    // Bind class declarations
    fn visit_class_declaration(&mut self, node: &ClassDeclarationElement) {
        let ident = &node.ident;
        let name = ident.value.clone();
        if self.bindings_table.lookup(&name).is_some() {
            // if we've seen this class before report and ignore it
            let error = SemanticError::ClassAlreadyDefined {
                name: name.clone(),
                span: ident.span.to_owned(),
            };
            self.errors.push(error);
        } else {
            self.bindings_context.push(ClassBinding {
                name: name.clone(),
                fields: vec![],
                methods: vec![],
            });

            walk_class_declaration(self, node);

            let binding = self.bindings_context.pop().unwrap();
            self.bindings_table.insert(name, binding);
        }
    }

    fn visit_class_field_declaration(&mut self, node: &ClassFieldElement) {
        let binding = self.bindings_context.last_mut().unwrap();
        binding.fields.push(Field {
            name: node.ident.value.clone(),
            index: binding.fields.len(),
            size: 4, // TODO compute type size
            ty: node.type_annotation.ty.clone(),
        });
        walk_class_field_declaration(self, node);
    }

    fn visit_class_method_declaration(&mut self, node: &ClassMethodElement) {
        let binding = self.bindings_context.last_mut().unwrap();
        let returns = node
            .returns
            .as_ref()
            .map(|it| it.ty.clone())
            .unwrap_or(Type::Binding("void".into()));

        let params = node
            .params
            .parameters
            .iter()
            .map(|param| param.type_annotation.ty.clone())
            .collect();

        binding.methods.push(Method {
            name: node.ident.value.clone(),
            signature: FunctionSignature { params, returns },
        });

        walk_class_method_declaration(self, node);
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use jswt_assert::assert_debug_snapshot;
    use jswt_parser::Parser;
    use jswt_symbols::SymbolTable;
    use jswt_tokenizer::Tokenizer;

    #[test]
    fn test_global_resolver_resolves_class_binding() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_global_resolver_resolves_class_binding",
            r"
        class Array {
            a: i32;
            b: i32;

            constructor(a: i32, b: f32) {}
            size(): i32 {}
            indexOf(a: i32): i32 {}
            
        }
        ",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut symbols = SymbolTable::default();
        let mut bindings = BindingsTable::default();
        let mut resolver = GlobalResolver::new(&mut bindings, &mut symbols);
        resolver.resolve(&ast);

        assert_eq!(resolver.errors.len(), 0);
        assert_debug_snapshot!(resolver);
    }

    #[test]
    fn test_global_resolver_resolves_function_bindings() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_global_resolver_resolves_function_bindings",
            r"
        function test(a: i32, b: i32) {
            let x = 99;
        }
 
        function test2(): Array {

        }
        ",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut symbols = SymbolTable::default();
        let mut bindings = BindingsTable::default();
        let mut resolver = GlobalResolver::new(&mut bindings, &mut symbols);
        resolver.resolve(&ast);

        assert_eq!(resolver.errors.len(), 0);
        assert_debug_snapshot!(resolver);
    }

    #[test]
    fn test_global_resolver_resolves_global_variables() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_global_resolver_resolves_global_variables",
            r"
        const PI = 3.14;

        function test(a: i32, b: i32) {
            let x = 99;
        }
 
        function test2(): Array {

        }
        ",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut symbols = SymbolTable::default();
        let mut bindings = BindingsTable::default();
        let mut resolver = GlobalResolver::new(&mut bindings, &mut symbols);
        resolver.resolve(&ast);

        assert_eq!(resolver.errors.len(), 0);
        assert_debug_snapshot!(resolver);
    }

    #[test]
    fn test_global_resolver_resolves_global_variables_with_duplicate_function_error() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_global_resolver_resolves_global_variables_with_duplicate_function_error",
            r"
        function test2(): Array {

        }
        function test2(): Array {

        }
        ",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut symbols = SymbolTable::default();
        let mut bindings = BindingsTable::default();
        let mut resolver = GlobalResolver::new(&mut bindings, &mut symbols);
        resolver.resolve(&ast);

        assert_debug_snapshot!(resolver);
    }

    #[test]
    fn test_global_resolver_resolves_class_binding_with_duplicate_class_error() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_global_resolver_resolves_class_binding_with_duplicate_class_error",
            r"
            class Array {
            }
            class Array {
            }
        ",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut symbols = SymbolTable::default();
        let mut bindings = BindingsTable::default();
        let mut resolver = GlobalResolver::new(&mut bindings, &mut symbols);
        resolver.resolve(&ast);

        assert_debug_snapshot!(resolver);
    }
}
