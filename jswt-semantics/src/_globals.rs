use jswt_ast::*;
use jswt_types::{FunctionType, Type};

use crate::{
    symbols::{FunctionBinding, Symbol, SymbolTable, ClassBinding},
    SemanticError,
};

#[derive(Debug)]
pub struct GlobalResolver {
    pub symbols: SymbolTable,
    pub errors: Vec<SemanticError>,
}

impl Default for GlobalResolver {
    fn default() -> Self {
        Self {
            symbols: SymbolTable::default(),
            errors: vec![],
        }
    }
}

impl GlobalResolver {
    pub fn resolve(&mut self, ast: &Ast) {
        self.visit_program(&ast.program);
    }
}

impl StatementVisitor for GlobalResolver {
    fn visit_program(&mut self, node: &Program) {
        // Push the global scope
        self.symbols.push_global_scope();

        for file in &node.files {
            self.visit_file(file)
        }

        self.symbols.pop_scope()
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

    fn visit_statement_element(&mut self, node: &StatementElement) {
        match node {
            StatementElement::Block(stmt) => self.visit_block_statement(stmt),
            StatementElement::Empty(_) => {}
            StatementElement::Return(_) => {}
            StatementElement::Variable(stmt) => self.visit_variable_statement(stmt),
            StatementElement::Expression(_) => {}
            StatementElement::If(_) => {}
            StatementElement::Iteration(_) => {}
        };
    }

    fn visit_block_statement(&mut self, _: &BlockStatement) {
        unreachable!()
    }

    fn visit_empty_statement(&mut self, _: &EmptyStatement) {
        unreachable!()
    }

    fn visit_if_statement(&mut self, _: &IfStatement) {
        unreachable!()
    }

    fn visit_iteration_statement(&mut self, _: &IterationStatement) {
        unreachable!()
    }

    fn visit_while_iteration_element(&mut self, _: &WhileIterationElement) {
        unreachable!()
    }

    fn visit_return_statement(&mut self, _: &ReturnStatement) {
        unreachable!()
    }

    fn visit_variable_statement(&mut self, _: &VariableStatement) {
        // add to global symbol table
    }

    fn visit_expression_statement(&mut self, _: &ExpressionStatement) {
        unreachable!()
    }

    fn visit_statement_list(&mut self, node: &StatementList) {
        for statement in &node.statements {
            self.visit_statement_element(statement);
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
            .unwrap_or(Type::Void);

        let params = node
            .params
            .parameters
            .iter()
            .map(|param| param.type_annotation.ty.clone())
            .collect();

        if self.symbols.lookup_current(&name).is_some() {
            let error = SemanticError::FunctionAlreadyDefined {
                name: name.clone(),
                span: ident.span.to_owned(),
            };
            self.errors.push(error);
        }

        self.symbols
            .define(name.clone(), FunctionBinding { params, returns });
    }

    fn visit_class_declaration(&mut self, node: &ClassDeclarationElement) {
        // Define a new type binding
        let name = &node.ident.value;
        self.symbols.define(name.clone(), ClassBinding {})

        // TODO
        // push a new scope in the symbol table
        // visit methods on the class.
        // Before popping the methods from the scope
        // add them to the class binding
    }

    fn visit_function_body(&mut self, _: &FunctionBody) {
        unreachable!()
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
        class Array {

        }

        function test() {
            let x = 99;
        }
 
        function test2(): Array {

        }
        ",
        );
        let ast = Parser::new(&mut tokenizer).parse();
        let mut resolver = GlobalResolver::default();
        resolver.resolve(&ast);
        assert_debug_snapshot!(resolver);
    }
}
