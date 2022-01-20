mod class_desugaring;

use self::class_desugaring::ClassDesugaring;
use jswt_ast::*;

#[derive(Debug, Default)]
pub struct AstDesugaring {
    class: ClassDesugaring,
}

impl AstDesugaring {
    pub fn desugar(&mut self, ast: &mut Ast) {
        self.visit_program(&mut ast.program);
    }
}

impl MutProgramVisitor<()> for AstDesugaring {
    fn visit_program(&mut self, node: &mut Program) {
        for file in &mut node.files {
            self.visit_file(file)
        }
    }

    fn visit_file(&mut self, node: &mut File) {
        self.visit_source_elements(&mut node.source_elements);
    }

    fn visit_source_elements(&mut self, node: &mut SourceElements) {
        self.class.enter_source_elements(node);
        for element in &mut node.source_elements {
            self.visit_source_element(element);
        }
        self.class.exit_source_elements(node);
    }

    fn visit_source_element(&mut self, node: &mut SourceElement) {
        match node {
            SourceElement::FunctionDeclaration(elem) => self.visit_function_declaration(elem),
            SourceElement::ClassDeclaration(elem) => {
                self.visit_class_declaration(elem);
            }

            SourceElement::Statement(elem) => self.visit_statement_element(elem),
        }
    }
}

impl MutStatementVisitor<()> for AstDesugaring {
    fn visit_statement_element(&mut self, node: &mut StatementElement) {
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

    fn visit_block_statement(&mut self, node: &mut BlockStatement) {
        self.visit_statement_list(&mut node.statements);
    }

    fn visit_empty_statement(&mut self, node: &mut EmptyStatement) {
        // No-op
    }

    fn visit_if_statement(&mut self, node: &mut IfStatement) {
        // No-op
    }

    fn visit_iteration_statement(&mut self, node: &mut IterationStatement) {
        // No-op
    }

    fn visit_while_iteration_element(&mut self, node: &mut WhileIterationElement) {
        // No-op
    }

    fn visit_return_statement(&mut self, node: &mut ReturnStatement) {
        // No-op
    }

    fn visit_variable_statement(&mut self, node: &mut VariableStatement) {
        // match &mut node.target {
        //     AssignableElement::Identifier(elem) => {
        //         elem.value = format!("{}/{}", node.span.module, elem.value).into();
        //     }
        // }
    }

    fn visit_expression_statement(&mut self, node: &mut ExpressionStatement) {
        // No-op
    }

    fn visit_statement_list(&mut self, node: &mut StatementList) {
        // No-op
    }

    fn visit_function_declaration(&mut self, node: &mut FunctionDeclarationElement) {
        // let ident = &mut node.ident;
        // ident.value = format!("{}/{}", node.span.module, ident.value).into();
    }

    fn visit_function_body(&mut self, node: &mut FunctionBody) {}

    fn visit_class_declaration(&mut self, node: &mut ClassDeclarationElement) {
        self.class.enter_class_declaration(node);
        self.visit_class_body(&mut node.body);
        self.class.exit_class_declaration();
    }

    fn visit_class_body(&mut self, node: &mut ClassBody) -> () {
        for element in &mut node.class_elements {
            match element {
                ClassElement::Constructor(elem) => self.visit_class_constructor_declaration(elem),
                ClassElement::Method(elem) => self.visit_class_method_declaration(elem),
            }
        }
    }

    fn visit_class_constructor_declaration(&mut self, node: &mut ClassConstructorElement) -> () {}

    fn visit_class_method_declaration(&mut self, node: &mut ClassMethodElement) -> () {
        self.class.enter_class_method_declaration(node);
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
        const a = 55;

        function test() {
        }

        ",
        );
        let mut ast = Parser::new(&mut tokenizer).parse();
        let mut desugering = AstDesugaring::default();
        desugering.desugar(&mut ast);
        assert_debug_snapshot!(ast);
    }
}
