use jswt_ast::{
    visit::{self, Visitor},
    *,
};
use jswt_common::{Span, Spannable};
use jswt_symbols::{SemanticEnvironment, TypesTable};

#[derive(Debug)]
pub struct AstSerializer<'a> {
    environment: &'a SemanticEnvironment,
    content: String,
    indent: usize,
}

/// A Naive AST Serializer to serialize the AST to a string
/// This is used for debugging purposes and is not guaranteed to be
/// a valid code.
impl<'a> AstSerializer<'a> {
    pub fn new(environment: &'a SemanticEnvironment) -> Self {
        Self {
            environment,
            content: String::new(),
            indent: 0,
        }
    }

    pub fn serialze(&mut self, ast: &Ast) -> &String {
        self.content += "// @ts-nocheck\n";

        self.visit_program(&ast.program);
        &self.content
    }

    fn indent(&mut self) {
        self.content += &" ".repeat(self.indent * 4);
    }

    fn append_type(&mut self, span: Span) {
        self.content += "/* ";
        self.content += &self.environment.get_type(&span).to_string();
        self.content += " */"
    }
}

impl<'a> Visitor for AstSerializer<'a> {
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
        self.visit_formal_parameter_list(&node.params);
        self.content += ": ";
        if let Some(ret) = &node.returns {
            self.content += &ret.ty.to_string();
        } else {
            self.content += "void";
        }

        self.content += " ";
        self.visit_block_statement(&node.body);
    }

    fn visit_statement_element(&mut self, node: &StatementElement) {
        self.indent();
        visit::walk_statement_element(self, node);
    }

    fn visit_block_statement(&mut self, node: &BlockStatement) {
        self.content += "{ \n";
        self.indent += 1;
        visit::walk_block_statement(self, node);
        self.indent -= 1;
        self.indent();
        self.content += "}\n";
    }

    fn visit_return_statement(&mut self, node: &ReturnStatement) {
        self.content += "return ";
        visit::walk_return_statement(self, node);
        self.content += ";\n";
    }

    fn visit_expression_statement(&mut self, node: &ExpressionStatement) {
        visit::walk_expression_statement(self, node);
        self.content += ";\n";
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

    fn visit_variable_declaration(&mut self, node: &VariableDeclarationElement) {
        let modifier = match node.modifier {
            VariableModifier::Let(_) => "let",
            VariableModifier::Const(_) => "const",
        };

        self.content += modifier;
        self.content += " ";
        self.content += &node.name.value;
        self.content += ": ";
        self.content += &self.environment.get_type(&node.span()).to_string();
        self.content += " = ";
        self.visit_single_expression(&node.expression);
        self.content += ";";
        self.content += "\n";
    }

    fn visit_variable_statement(&mut self, node: &VariableStatement) {
        let modifier = match node.modifier {
            VariableModifier::Let(_) => "let",
            VariableModifier::Const(_) => "const",
        };

        self.content += modifier;
        self.content += " ";
        self.visit_assignable_element(&node.target);
        self.content += ": ";
        self.content += &self.environment.get_type(&node.span()).to_string();
        self.content += " = ";
        self.visit_single_expression(&node.expression);
        self.content += ";";
        self.content += "\n";
    }

    fn visit_assignable_element(&mut self, node: &AssignableElement) {
        match node {
            AssignableElement::Identifier(ident) => self.content += &ident.value,
        }
    }

    fn visit_argument_expression(&mut self, node: &ArgumentsExpression) {
        self.content += "(";
        self.visit_single_expression(&*node.ident);
        self.content += "(";
        for (i, arg) in node.arguments.arguments.iter().enumerate() {
            self.visit_single_expression(arg);
            if i + 1 != node.arguments.arguments.len() {
                self.content += ", ";
            }
        }
        self.content += ")";

        self.append_type(node.span());
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
        let name = &node.ident.value;
        self.content += name;
        self.content += "/* ";
        self.content += &self.environment.get_type(&node.span()).to_string();
        self.content += " */"
        // match self.environment.get_symbol(&name) {
        //     Some(Symbol::Variable(_)) => {
        //         self.content += "/* ";
        //         self.content += &self.environment.get_type(&node.span()).to_string();
        //         self.content += " */"
        //     }
        //     _ => {}
        // }
    }

    fn visit_literal(&mut self, node: &Literal) {
        match node {
            Literal::Array(_) => todo!(),
            Literal::String(s) => self.content += s.value,
            Literal::Integer(i) => self.content += &i.value.to_string(),
            Literal::Float(f) => self.content += &f.value.to_string(),
            Literal::Boolean(b) => self.content += &b.value.to_string(),
        }
        self.content += "/* ";
        self.content += &self.environment.get_type(&node.span()).to_string();
        self.content += " */"
    }

    fn visit_class_declaration(&mut self, node: &ClassDeclarationElement) {
        self.content += "class ";
        self.content += &node.ident.value;
        self.content += " ";
        visit::walk_class_declaration(self, node);
        self.content += "\n";
    }

    fn visit_class_body(&mut self, node: &ClassBody) {
        self.content += "{";
        self.indent += 1;
        self.content += "\n";
        visit::walk_class_body(self, node);
        self.indent -= 1;
        self.content += "}\n";
    }

    fn visit_class_constructor_declaration(&mut self, node: &ClassConstructorElement) {
        self.indent();
        self.content += "constructor";
        visit::walk_class_constructor_declaration(self, node);
    }

    fn visit_class_method_declaration(&mut self, node: &ClassMethodElement) {
        self.indent();
        self.content += &node.ident.value;
        self.content += " ";
        visit::walk_class_method_declaration(self, node);
    }

    fn visit_formal_parameter_list(&mut self, node: &FormalParameterList) {
        self.content += "(";
        for (i, arg) in node.parameters.iter().enumerate() {
            self.visit_formal_parameter_arg(arg);
            if i + 1 != node.parameters.len() {
                self.content += ", ";
            }
        }
        self.content += ") ";
    }

    fn visit_formal_parameter_arg(&mut self, node: &FormalParameterArg) {
        self.content += &node.ident.value;
        self.content += ": ";
        self.content += &self.environment.get_type(&node.span()).to_string();
    }
}
