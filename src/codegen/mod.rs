mod symbols;
use crate::ast::{program::*, Ast};
use crate::wasm::module::Module;

impl Module {
    pub fn evaluate(&mut self, ast: &Ast) -> &mut Module {
        self.visit_program(&ast.program)
    }

    fn visit_program(&mut self, node: &Program) -> &mut Module {
        self.visit_source_elements(&node.source_elements)
    }

    fn visit_source_elements(&mut self, node: &SourceElements) -> &mut Module {
        for element in &node.source_elements {
            self.visit_source_element(element);
        }
        self
    }

    fn visit_source_element(&mut self, node: &SourceElement) -> &mut Module {
        match node {
            SourceElement::FunctionDeclaration(func) => self.visit_function_declaration(func),
            SourceElement::Statement(stmt) => self.visit_statement_element(stmt),
        }
    }

    fn visit_statement_element(&mut self, node: &StatementElement) -> &mut Module {
        match node {
            StatementElement::Block(blk) => self.visit_block_statement(blk),
            StatementElement::Empty(empty) => self.visit_empty_statement(empty),
            StatementElement::Return(ret) => self.visit_return_statement(ret),
            StatementElement::Variable(var) => self.visit_variable_statement(var),
        }
    }

    fn visit_block_statement(&mut self, node: &BlockStatement) -> &mut Module {
        self.visit_statement_list(&node.statements)
    }

    fn visit_empty_statement(&mut self, _node: &EmptyStatement) -> &mut Module {
        // Nothing to do with an empty statement
        self
    }

    fn visit_return_statement(&mut self, node: &ReturnStatement) -> &mut Module {
        self.visit_single_expression(&node.expression)
    }

    fn visit_variable_statement(&mut self, node: &VariableStatement) -> &mut Module {
        todo!()
    }

    fn visit_statement_list(&mut self, node: &StatementList) -> &mut Module {
        todo!()
    }

    fn visit_function_declaration(&mut self, node: &FunctionDeclarationElement) -> &mut Module {
        todo!()
    }

    fn visit_assignable_element(&mut self, node: &AssignableElement) -> &mut Module {
        todo!()
    }

    fn visit_single_expression(&mut self, node: &SingleExpression) -> &mut Module {
        todo!()
    }

    fn visit_literal(&mut self, node: &Literal) -> &mut Module {
        todo!()
    }
}
