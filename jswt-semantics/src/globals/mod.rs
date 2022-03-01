mod symbols;

use jswt_ast::{visit::Visitor, *};
use jswt_symbols::SymbolTable;

use self::symbols::*;
use crate::SemanticError;

#[derive(Debug)]
pub struct GlobalResolver<'a> {
    symbols: GlobalSymbolsResolver<'a>,
}

impl<'a> GlobalResolver<'a> {
    pub fn new(symbols: &'a mut SymbolTable) -> Self {
        Self {
            symbols: GlobalSymbolsResolver::new(symbols),
        }
    }

    pub fn resolve(&mut self, ast: &Ast) {
        self.visit_program(&ast.program);
    }

    pub fn errors(&mut self) -> Vec<SemanticError> {
        let mut errors = vec![];
        errors.append(&mut self.symbols.errors);
        errors
    }
}

impl<'a> Visitor for GlobalResolver<'a> {
    fn visit_program(&mut self, node: &Program) {
        self.symbols.enter_program(node);
        visit::walk_program(self, node);
        self.symbols.exit_program(node);
    }

    fn visit_function_declaration(&mut self, node: &FunctionDeclarationElement) {
        self.symbols.enter_function_declaration(node);
        visit::walk_function_declaration(self, node);
        self.symbols.exit_function_declaration(node);
    }

    fn visit_class_declaration(&mut self, node: &ClassDeclarationElement) {
        self.symbols.enter_class_declaration(node);
        visit::walk_class_declaration(self, node);
        self.symbols.exit_class_declaration(node);
    }

    fn visit_class_constructor_declaration(&mut self, node: &ClassConstructorElement) {
        self.symbols.enter_constructor_declaration(node);
        visit::walk_class_constructor_declaration(self, node);
    }

    fn visit_class_method_declaration(&mut self, node: &ClassMethodElement) {
        self.symbols.enter_method_declaration(node);
        visit::walk_class_method_declaration(self, node);
    }

    fn visit_class_field_declaration(&mut self, node: &ClassFieldElement) {
        self.symbols.enter_field_declaration(node);
        visit::walk_class_field_declaration(self, node);
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use jswt_assert::assert_debug_snapshot;
    use jswt_parser::Parser;
    use jswt_tokenizer::Tokenizer;

    #[test]
    #[ignore]
    fn test_global_resolver_resolves_class_binding() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_global_resolver_resolves_class_binding",
            r"
        let global = 55;

        class Array {
            a: i32;
            b: i32;

            constructor(a: i32, b: f32) {}
            method() {}
        }

        function test() {
            let x = 99;
        }
 
        function doSomething(): Array { }
        ",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut symbols = SymbolTable::default();
        let mut resolver = GlobalResolver::new(&mut symbols);
        resolver.resolve(&ast);
        assert_debug_snapshot!(resolver);
    }

    #[test]
    #[ignore]
    fn test_global_resolver_resolves_function_bindings() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test_global_resolver_resolves_function_bindings",
            r"
        class Array {

        }

        function test(a: i32, b: i32) {
            let x = 99;
        }
 
        function test2(): Array {

        }
        ",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut symbols = SymbolTable::default();
        let mut resolver = GlobalResolver::new(&mut symbols);
        resolver.resolve(&ast);
        assert_debug_snapshot!(resolver);
    }
}
