mod class;
mod gen;

use std::borrow::Cow;

use gen::{parse_expr, parse_statement};
use jswt_ast::*;
use jswt_common::{Span, Spannable};
use jswt_symbols::SymbolTable;

#[derive(Debug)]
pub struct AstLowering<'a> {
    symbols: &'a mut SymbolTable,
    binding_context: Option<Cow<'static, str>>,
}

impl<'a> AstLowering<'a> {
    pub fn new(symbols: &'a mut SymbolTable) -> Self {
        Self {
            symbols,
            binding_context: None,
        }
    }

    pub fn desugar(&mut self, ast: &mut Ast) {
        let program = self.program(&ast.program);
        ast.program = program;
    }
}

impl<'a> AstLowering<'a> {
    fn program(&mut self, program: &Program) -> Program {
        self.symbols.push_global_scope();
        let mut files = vec![];
        for file in &program.files {
            files.push(self.file(file));
        }
        self.symbols.pop_scope();
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
        let body = self.block_statement(&node.body);
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

    fn block_statement(&mut self, node: &BlockStatement) -> BlockStatement {
        self.symbols.push_scope(node.span());
        let statements = self.statement_list(&node.statements);
        self.symbols.pop_scope();
        BlockStatement {
            span: node.span(),
            statements,
        }
    }

    fn statement_list(&mut self, node: &StatementList) -> StatementList {
        let mut statements = vec![];
        for statement in &node.statements {
            for element in self.statement_element(statement) {
                if let SourceElement::Statement(element) = element {
                    statements.push(element);
                };
            }
        }
        StatementList { statements }
    }

    fn statement_element(&mut self, node: &StatementElement) -> Vec<SourceElement> {
        let element = match &node {
            StatementElement::Expression(stmt) => self.expression_statement(stmt),
            StatementElement::Variable(stmt) => self.variable_statement(stmt),
            _ => node.clone(),
        };
        vec![SourceElement::Statement(element)]
    }

    fn variable_statement(&mut self, node: &VariableStatement) -> StatementElement {
        StatementElement::Variable(VariableStatement {
            span: node.span(),
            modifier: node.modifier.clone(),
            target: node.target.clone(),
            expression: self.expression(&node.expression),
            type_annotation: node.type_annotation.clone(),
        })
    }

    fn expression_statement(&mut self, node: &ExpressionStatement) -> StatementElement {
        let expression = self.expression(&node.expression);
        StatementElement::Expression(ExpressionStatement {
            span: node.span(),
            expression,
        })
    }

    fn expression(&mut self, node: &SingleExpression) -> SingleExpression {
        self.visit_single_expression(node)
    }

    fn class_declaration(&mut self, node: &ClassDeclarationElement) -> Vec<SourceElement> {
        self.enter_class_declaration(node);
        let elements = self.class_body(&node.body);
        self.exit_class_declaration();
        elements
    }

    fn class_body(&mut self, node: &ClassBody) -> Vec<SourceElement> {
        let mut elements = vec![];
        for element in &node.class_elements {
            elements.push(match element {
                ClassElement::Constructor(elem) => self.class_constructor_declaration(elem),
                ClassElement::Method(elem) => self.class_method_declaration(elem),
                ClassElement::Field(elem) => self.class_field_declaration(elem),
            });
        }
        elements
    }

    fn class_constructor_declaration(&mut self, node: &ClassConstructorElement) -> SourceElement {
        self.enter_class_constructor(node)
    }

    fn class_method_declaration(&mut self, node: &ClassMethodElement) -> SourceElement {
        self.enter_class_method(node)
    }

    fn class_field_declaration(&mut self, node: &ClassFieldElement) -> SourceElement {
        SourceElement::Statement(StatementElement::Empty(EmptyStatement {
            span: node.span(),
        }))
    }
}

impl<'a> ExpressionVisitor<SingleExpression> for AstLowering<'a> {
    fn visit_single_expression(&mut self, node: &SingleExpression) -> SingleExpression {
        match node {
            SingleExpression::Unary(expr) => self.visit_unary_expression(expr),
            SingleExpression::Assignment(expr) => self.visit_assignment_expression(expr),
            SingleExpression::MemberIndex(expr) => self.visit_member_index(expr),
            SingleExpression::New(expr) => self.visit_new(expr),
            SingleExpression::Arguments(expr) => self.visit_argument_expression(expr),
            SingleExpression::Multiplicative(expr)
            | SingleExpression::Bitwise(expr)
            | SingleExpression::Additive(expr)
            | SingleExpression::Equality(expr)
            | SingleExpression::Relational(expr) => self.visit_binary_expression(expr),
            SingleExpression::Identifier(expr) => self.visit_identifier_expression(expr),
            SingleExpression::MemberDot(expr) => self.visit_member_dot(expr),
            SingleExpression::This(expr) => self.visit_this_expression(expr),
            SingleExpression::Literal(_) => node.clone(),
        }
    }

    fn visit_assignable_element(&mut self, _: &AssignableElement) -> SingleExpression {
        todo!()
    }

    fn visit_member_dot(&mut self, node: &MemberDotExpression) -> SingleExpression {
        // We only support dot expressions on idents for now
        let target = node.target.as_identifier().unwrap();

        // Figure out the type binding for the identifier
        let binding = self
            .symbols
            .lookup_class_binding(target.ident.value.clone())
            .unwrap();

        // if let SingleExpression::This(..) = &*node.target {
        //     // this is a class context - we want to rewrite this to an i32load
        // }

        match &*node.expression {
            SingleExpression::Identifier(args) => {
                let name = &args.ident.value;
                let field = binding.field(&name).unwrap();
                parse_expr(format!("i32Load({} + {} *4)", name, field.index))
            }
            SingleExpression::Arguments(args) => {
                // This is a method call on the bound type
                // Rewrite the method call
                let name = args.ident.as_identifier().unwrap();

                // Generate argument list including the target pointer as the first param
                let mut arguments = vec![*node.target.clone()];
                for arg in &args.arguments.arguments {
                    arguments.push(arg.clone())
                }

                SingleExpression::Arguments(ArgumentsExpression {
                    span: node.span(),
                    ident: Box::new(SingleExpression::Identifier(IdentifierExpression {
                        span: Span::synthetic(),
                        ident: Identifier {
                            span: Span::synthetic(),
                            value: format!("{}#{}", &binding.name, &name.ident.value).into(),
                        },
                    })),
                    arguments: ArgumentsList {
                        span: args.span(),
                        arguments,
                    },
                })
            }
            _ => unreachable!(),
        }
    }

    fn visit_member_index(&mut self, node: &MemberIndexExpression) -> SingleExpression {
        // rewrite to get function call

        todo!();
        // SingleExpression::Arguments(ArgumentsExpression {
        //     span: node.span,
        //     ident: node.target.clone(),
        //     arguments: todo!(),
        // })
    }

    fn visit_new(&mut self, node: &NewExpression) -> SingleExpression {
        // rewrite new as a function call invoking the lowered synthetic
        // constructor declaration of the class
        let mut args = node.expression.as_arguments().unwrap().clone();
        let mut ident = args.ident.as_identifier_mut().unwrap();
        ident.ident.value = format!("{}#constructor", ident.ident.value).into();

        SingleExpression::Arguments(args)
    }

    fn visit_identifier_expression(&mut self, node: &IdentifierExpression) -> SingleExpression {
        jswt_ast::SingleExpression::Identifier(node.clone())
    }

    fn visit_argument_expression(&mut self, node: &ArgumentsExpression) -> SingleExpression {
        
        SingleExpression::Arguments(ArgumentsExpression {
            span: node.span(),
            ident: node.ident.clone(),
            arguments: node.arguments.clone(),
        })
    }

    fn visit_unary_expression(&mut self, node: &UnaryExpression) -> SingleExpression {
        SingleExpression::Unary(UnaryExpression {
            expr: node.expr.clone(),
            op: node.op.clone(),
            span: node.span(),
        })
    }

    fn visit_assignment_expression(&mut self, node: &BinaryExpression) -> SingleExpression {
        if let SingleExpression::MemberDot(dot) = &*node.left {
            if let SingleExpression::This(_) = &*dot.target {
                // this binding in class.
                // this.expression = value;

                // This is always an identifier
                let target = dot.expression.as_identifier().unwrap();
                let value = &*node.right;
                return self.class_this_field_assignment(target, value);
            }
        }

        SingleExpression::Assignment(BinaryExpression {
            span: node.span(),
            left: Box::new(self.visit_single_expression(&node.left)),
            op: node.op.clone(),
            right: Box::new(self.visit_single_expression(&node.right)),
        })
    }

    fn visit_binary_expression(&mut self, node: &BinaryExpression) -> SingleExpression {
        // node.left
        todo!()
    }

    fn visit_this_expression(&mut self, _: &ThisExpression) -> SingleExpression {
        unreachable!()
    }

    fn visit_literal(&mut self, _: &Literal) -> SingleExpression {
        unreachable!()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use jswt_assert::assert_debug_snapshot;
    use jswt_parser::Parser;
    use jswt_semantics::SemanticAnalyzer;
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
        let mut analyzer = SemanticAnalyzer::default();
        analyzer.analyze(&mut ast);

        let mut lowering = AstLowering::new(&mut analyzer.symbol_table);
        lowering.desugar(&mut ast);

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
        let mut analyzer = SemanticAnalyzer::default();
        analyzer.analyze(&mut ast);

        let mut lowering = AstLowering::new(&mut analyzer.symbol_table);
        lowering.desugar(&mut ast);

        assert_debug_snapshot!(ast);
    }
}
