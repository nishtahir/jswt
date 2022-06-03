use jswt_ast::{
    visit::{self, Visitor},
    *,
};

#[derive(Default)]
pub struct AstSerializer {
    content: String,
    indent: usize,
}

impl AstSerializer {
    fn indent(&mut self) {
        self.content += &" ".repeat(self.indent * 4);
    }
}

impl AstSerializer {
    pub fn serialze(&mut self, ast: &Ast) -> &String {
        self.content += "// @ts-nocheck\n";

        self.visit_program(&ast.program);
        &self.content
    }
}

impl Visitor for AstSerializer {
    fn visit_file(&mut self, node: &File) {
        self.content += "\n";
        self.content += "// ";
        self.content += &node.span.file;
        self.content += "\n\n";
        visit::walk_file(self, node);
    }

    fn visit_function_declaration(&mut self, node: &FunctionDeclarationElement) {
        for annotation in node.decorators.annotations.iter() {
            self.content += "@";
            self.content += &annotation.name.value;
            self.content += "(";
            self.content += &format!("{:?}", annotation.expr);
            self.content += ")";
            self.content += "\n";
            self.indent()
        }

        self.content += "function ";
        self.content += &node.ident.value;
        self.content += "(";

        for (i, param) in node.params.parameters.iter().enumerate() {
            self.content += &param.ident.value;
            self.content += ": ";
            self.content += &param.type_annotation.ty.to_string();
            if i + 1 != node.params.parameters.len() {
                self.content += ", "
            }
        }
        self.content += ")";
        if let Some(ret) = &node.returns {
            self.content += ": ";
            self.content += ret.ty.to_string();
        }

        self.content += " ";
        visit::walk_function_declaration(self, node);
    }

    fn visit_statement_element(&mut self, node: &StatementElement) {
        self.indent();
        visit::walk_statement_element(self, node);
        self.content += ";\n";
    }

    fn visit_block_statement(&mut self, node: &BlockStatement) {
        self.content += "{ \n";
        self.indent += 1;
        visit::walk_block_statement(self, node);
        self.indent -= 1;
        self.content += "}\n";
    }

    fn visit_return_statement(&mut self, node: &ReturnStatement) {
        self.content += "return ";
        visit::walk_return_statement(self, node);
    }

    fn visit_unary_expression(&mut self, node: &UnaryExpression) {
        match node.op {
            UnaryOperator::Plus(_) => {
                self.content += "+";
                self.visit_single_expression(&node.expr);
            }
            UnaryOperator::Minus(_) => {
                self.content += "-";
                self.visit_single_expression(&node.expr);
            }
            UnaryOperator::Not(_) => {
                self.content += "!";
                self.visit_single_expression(&node.expr);
            }
            UnaryOperator::PostIncrement(_) => {
                self.visit_single_expression(&node.expr);
                self.content += "++";
            }
            UnaryOperator::PostDecrement(_) => {
                self.visit_single_expression(&node.expr);
                self.content += "--";
            }
        };
    }

    fn visit_member_dot(&mut self, node: &MemberDotExpression) {
        self.visit_single_expression(&node.target);
        self.content += ".";
        self.visit_single_expression(&node.expression);
    }

    fn visit_variable_statement(&mut self, node: &VariableStatement) {
        let modifier = match node.modifier {
            VariableModifier::Let(_) => "let",
            VariableModifier::Const(_) => "const",
        };

        self.content += modifier;
        self.content += " ";
        self.visit_assignable_element(&node.target);
        self.content += " = ";
        self.visit_single_expression(&node.expression);
    }

    fn visit_assignable_element(&mut self, node: &AssignableElement) {
        match node {
            AssignableElement::Identifier(ident) => self.content += &ident.value,
        }
    }

    fn visit_argument_expression(&mut self, node: &ArgumentsExpression) {
        self.visit_single_expression(&*node.ident);
        self.content += "(";
        for (i, arg) in node.arguments.arguments.iter().enumerate() {
            self.visit_single_expression(arg);
            if i + 1 != node.arguments.arguments.len() {
                self.content += ", ";
            }
        }
        self.content += ")";
    }

    fn visit_assignment_expression(&mut self, node: &BinaryExpression) {
        self.visit_binary_expression(node);
    }

    fn visit_binary_expression(&mut self, node: &BinaryExpression) {
        let op = match node.op {
            BinaryOperator::Plus(_) => "+",
            BinaryOperator::Minus(_) => "-",
            BinaryOperator::Mult(_) => "*",
            BinaryOperator::Div(_) => "/",
            BinaryOperator::Equal(_) => "==",
            BinaryOperator::NotEqual(_) => "!=",
            BinaryOperator::Greater(_) => ">",
            BinaryOperator::GreaterEqual(_) => ">=",
            BinaryOperator::Less(_) => "<",
            BinaryOperator::LessEqual(_) => "<=",
            BinaryOperator::And(_) => "&",
            BinaryOperator::Or(_) => "|",
            BinaryOperator::Assign(_) => "=",
        };
        self.visit_single_expression(&node.left);
        self.content += " ";
        self.content += op;
        self.content += " ";
        self.visit_single_expression(&node.right);
    }

    fn visit_this_expression(&mut self, _: &ThisExpression) {
        self.content += "this";
    }

    fn visit_identifier_expression(&mut self, node: &IdentifierExpression) {
        self.content += &node.ident.value;
    }

    fn visit_literal(&mut self, node: &Literal) {
        match node {
            Literal::Array(_) => todo!(),
            Literal::String(s) => self.content += s.value,
            Literal::Integer(i) => self.content += &i.value.to_string(),
            Literal::Float(f) => self.content += &f.value.to_string(),
            Literal::Boolean(b) => self.content += &b.value.to_string(),
        }
    }
}
