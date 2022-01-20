mod class_desugaring;

use class_desugaring::*;
use jswt_ast::*;
use jswt_common::Spannable;

#[derive(Debug, Default)]
pub struct AstDesugaring {
    class: ClassDesugaring,
}

impl AstDesugaring {
    pub fn desugar(&mut self, ast: &mut Ast) {
        let program = self.program(&ast.program);
        ast.program = program;
    }
}

impl AstDesugaring {
    fn program(&mut self, program: &Program) -> Program {
        let mut files = vec![];
        for file in &program.files {
            files.push(self.file(file));
        }

        Program { files }
    }

    fn file(&mut self, file: &File) -> File {
        let source_elements = self.source_elements(&file.source_elements);
        File { source_elements }
    }

    fn source_elements(&mut self, node: &SourceElements) -> SourceElements {
        let mut source_elements = vec![];
        for element in &node.source_elements {
            let mut elements = self.source_element(element);
            source_elements.append(&mut elements);
        }
        SourceElements { source_elements }
    }

    // It's possible for an element to be lowered to a list of elements
    // for example a Class => Vec<Function>
    fn source_element(&mut self, node: &SourceElement) -> Vec<SourceElement> {
        match node {
            SourceElement::FunctionDeclaration(elem) => self.function_declaration(elem),
            SourceElement::Statement(elem) => self.statement_element(elem),
            SourceElement::ClassDeclaration(elem) => self.class_declaration(elem),
        }
    }

    fn function_declaration(&mut self, node: &FunctionDeclarationElement) -> Vec<SourceElement> {
        let body = self.function_body(&node.body);
        vec![SourceElement::FunctionDeclaration(
            FunctionDeclarationElement {
                span: node.span(),
                decorators: node.decorators.clone(),
                ident: node.ident.clone(),
                params: node.params.clone(),
                returns: node.returns.clone(),
                body,
            },
        )]
    }

    fn function_body(&mut self, node: &FunctionBody) -> FunctionBody {
        FunctionBody {
            span: node.span(),
            source_elements: self.source_elements(&node.source_elements),
        }
    }

    fn statement_element(&self, node: &StatementElement) -> Vec<SourceElement> {
        let element = match &node {
            StatementElement::Expression(stmt) => self.expression_statement(stmt),
            StatementElement::Variable(stmt) => self.variable_statement(stmt),
            _ => node.clone(),
        };
        vec![SourceElement::Statement(element)]
    }

    fn variable_statement(&self, node: &VariableStatement) -> StatementElement {
        StatementElement::Variable(VariableStatement {
            span: node.span(),
            modifier: node.modifier.clone(),
            target: node.target.clone(),
            expression: self.expression(&node.expression),
            type_annotation: node.type_annotation.clone(),
        })
    }

    fn expression_statement(&self, node: &ExpressionStatement) -> StatementElement {
        let expression = self.expression(&node.expression);
        StatementElement::Expression(ExpressionStatement {
            span: node.span(),
            expression,
        })
    }

    fn expression(&self, node: &SingleExpression) -> SingleExpression {
        match node {
            SingleExpression::New(expr) => self.new_expression(expr),
            _ => node.clone(),
        }
    }

    fn new_expression(&self, node: &NewExpression) -> SingleExpression {
        // TODO - make this safe. This is making a lot of assumptions
        let mut args = node.expression.as_arguments().unwrap().clone();
        let mut ident = args.ident.as_identifier_mut().unwrap();
        ident.ident.value = format!("{}#constructor", ident.ident.value).into();

        SingleExpression::Arguments(args)
    }

    fn class_declaration(&mut self, node: &ClassDeclarationElement) -> Vec<SourceElement> {
        self.class.enter_class_declaration(node);
        let elements = self.class_body(&node.body);
        self.class.exit_class_declaration();
        elements
    }

    fn class_body(&self, node: &ClassBody) -> Vec<SourceElement> {
        let mut elements = vec![];
        for element in &node.class_elements {
            elements.push(match element {
                ClassElement::Constructor(elem) => self.class_constructor_declaration(elem),
                ClassElement::Method(elem) => self.class_method_declaration(elem),
            });
        }
        elements
    }

    fn class_constructor_declaration(&self, node: &ClassConstructorElement) -> SourceElement {
        self.class.enter_class_constructor(node)
    }

    fn class_method_declaration(&self, node: &ClassMethodElement) -> SourceElement {
        self.class.enter_class_method(node)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use jswt_assert::assert_debug_snapshot;
    use jswt_parser::Parser;
    use jswt_tokenizer::Tokenizer;

    #[test]
    fn test_class_declaration_desugars_into_functions() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test.1",
            r"
            class Array {
                constructor(len: i32, capacity: i32) {

                }
            }
        ",
        );
        let mut ast = Parser::new(&mut tokenizer).parse();
        let mut desugering = AstDesugaring::default();
        desugering.desugar(&mut ast);
        assert_debug_snapshot!(ast);
    }

    #[test]
    fn test_class_declaration_desugars_new_expression() {
        let mut tokenizer = Tokenizer::default();
        tokenizer.enqueue_source_str(
            "test.1",
            r"
            class Array {
                constructor(len: i32, capacity: i32) {

                }
            }

            function main() {
                let x = new Array(1, 2);
            }
        ",
        );
        let mut ast = Parser::new(&mut tokenizer).parse();
        let mut desugering = AstDesugaring::default();
        desugering.desugar(&mut ast);
        assert_debug_snapshot!(ast);
    }
}
