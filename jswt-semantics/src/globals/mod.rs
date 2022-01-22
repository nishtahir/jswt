mod symbols;

use jswt_ast::*;
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

impl<'a> ProgramVisitor<()> for GlobalResolver<'a> {
    fn visit_program(&mut self, node: &Program) {
        self.symbols.enter_program(node);
        for file in &node.files {
            self.visit_file(file)
        }
        self.symbols.exit_program(node);
    }

    fn visit_file(&mut self, node: &File) {
        self.visit_source_elements(&node.source_elements);
    }

    fn visit_source_elements(&mut self, node: &SourceElements) {
        for element in &node.source_elements {
            self.visit_source_element(element);
        }
    }

    fn visit_source_element(&mut self, node: &SourceElement) {
        match node {
            SourceElement::FunctionDeclaration(elem) => self.visit_function_declaration(elem),
            SourceElement::ClassDeclaration(elem) => self.visit_class_declaration(elem),
            SourceElement::Statement(elem) => self.visit_statement_element(elem),
        }
    }
}

impl<'a> StatementVisitor<()> for GlobalResolver<'a> {
    fn visit_statement_element(&mut self, node: &StatementElement) {
        match node {
            StatementElement::Block(stmt) => self.visit_block_statement(stmt),
            StatementElement::Empty(stmt) => self.visit_empty_statement(stmt),
            StatementElement::Return(stmt) => self.visit_return_statement(stmt),
            StatementElement::Variable(stmt) => self.visit_variable_statement(stmt),
            StatementElement::Expression(stmt) => self.visit_expression_statement(stmt),
            StatementElement::If(stmt) => self.visit_if_statement(stmt),
            StatementElement::Iteration(_) => {}
        };
    }

    fn visit_block_statement(&mut self, _: &BlockStatement) {
        // No-op
    }

    fn visit_empty_statement(&mut self, _: &EmptyStatement) {
        // No-op
    }

    fn visit_if_statement(&mut self, _: &IfStatement) {
        // No-op
    }

    fn visit_iteration_statement(&mut self, _: &IterationStatement) {
        unreachable!()
    }

    fn visit_while_iteration_element(&mut self, _: &WhileIterationElement) {
        unreachable!()
    }

    fn visit_return_statement(&mut self, _: &ReturnStatement) {
        // No-op
    }

    fn visit_variable_statement(&mut self, _: &VariableStatement) {
        // No-op
    }

    fn visit_expression_statement(&mut self, _: &ExpressionStatement) {
        // No-op
    }

    fn visit_statement_list(&mut self, node: &StatementList) {
        for statement in &node.statements {
            self.visit_statement_element(statement);
        }
    }

    fn visit_function_declaration(&mut self, node: &FunctionDeclarationElement) {
        self.symbols.enter_function_declaration(node);
        self.visit_block_statement(&node.body);
        self.symbols.exit_function_declaration(node);
    }

    fn visit_class_declaration(&mut self, node: &ClassDeclarationElement) {
        self.symbols.enter_class_declaration(node);
        self.visit_class_body(&node.body);
        self.symbols.exit_class_declaration(node);
    }

    fn visit_class_constructor_declaration(&mut self, node: &ClassConstructorElement) {
        self.symbols.enter_constructor_declaration(node);
    }

    fn visit_class_method_declaration(&mut self, node: &ClassMethodElement) {
        self.symbols.enter_method_declaration(node);
    }

    fn visit_class_body(&mut self, node: &ClassBody) {
        for element in &node.class_elements {
            match element {
                ClassElement::Constructor(elem) => self.visit_class_constructor_declaration(elem),
                ClassElement::Method(elem) => self.visit_class_method_declaration(elem),
            }
        }
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use jswt_assert::assert_debug_snapshot;
    use jswt_parser::Parser;
    use jswt_tokenizer::Tokenizer;

    #[test]
    fn test_global_resolver_resolves_class_binding() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test.1",
            r"
        let global = 55;

        class Array {
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
    fn test_global_resolver_resolves_function_bindings() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test.1",
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
