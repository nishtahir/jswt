use crate::*;

pub trait Visitor: Sized {
    fn visit_program(&mut self, node: &Program) {
        walk_program(self, node);
    }

    fn visit_file(&mut self, node: &File) {
        walk_file(self, node);
    }

    fn visit_source_elements(&mut self, node: &SourceElements) {
        walk_source_elements(self, node);
    }

    fn visit_source_element(&mut self, node: &SourceElement) {
        walk_source_element(self, node);
    }

    fn visit_statement_element(&mut self, node: &StatementElement) {
        walk_statement_element(self, node);
    }

    fn visit_block_statement(&mut self, node: &BlockStatement) {
        walk_block_statement(self, node);
    }

    fn visit_empty_statement(&mut self, node: &EmptyStatement) {
        walk_empty_statement(self, node);
    }

    fn visit_if_statement(&mut self, node: &IfStatement) {
        walk_if_statement(self, node);
    }

    fn visit_iteration_statement(&mut self, node: &IterationStatement) {
        walk_iteration_statement(self, node);
    }

    fn visit_while_iteration_element(&mut self, node: &WhileIterationElement) {
        walk_while_iteration_element(self, node);
    }

    fn visit_return_statement(&mut self, node: &ReturnStatement) {
        walk_return_statement(self, node);
    }

    fn visit_variable_statement(&mut self, node: &VariableStatement) {
        walk_variable_statement(self, node);
    }

    fn visit_expression_statement(&mut self, node: &ExpressionStatement) {
        walk_expression_statement(self, node);
    }

    fn visit_statement_list(&mut self, node: &StatementList) {
        walk_statement_list(self, node);
    }

    fn visit_function_declaration(&mut self, node: &FunctionDeclarationElement) {
        walk_function_declaration(self, node);
    }

    fn visit_class_declaration(&mut self, node: &ClassDeclarationElement) {
        walk_class_declaration(self, node);
    }

    fn visit_variable_declaration(&mut self, node: &VariableDeclarationElement) {
        walk_variable_declaration(self, node);
    }

    fn visit_import_declaration(&mut self, node: &ImportDeclarationElement) {
        walk_import_declaration(self, node);
    }

    fn visit_class_body(&mut self, node: &ClassBody) {
        walk_class_body(self, node);
    }

    fn visit_class_constructor_declaration(&mut self, node: &ClassConstructorElement) {
        walk_class_constructor_declaration(self, node);
    }

    fn visit_class_method_declaration(&mut self, node: &ClassMethodElement) {
        walk_class_method_declaration(self, node);
    }

    fn visit_class_field_declaration(&mut self, node: &ClassFieldElement) {
        walk_class_field_declaration(self, node);
    }

    fn visit_single_expression(&mut self, node: &SingleExpression) {
        walk_single_expression(self, node);
    }

    fn visit_assignable_element(&mut self, node: &AssignableElement) {
        walk_assignable_element(self, node);
    }

    fn visit_member_dot(&mut self, node: &MemberDotExpression) {
        walk_member_dot(self, node);
    }

    fn visit_member_index(&mut self, node: &MemberIndexExpression) {
        walk_member_index(self, node);
    }

    fn visit_new(&mut self, node: &NewExpression) {
        walk_new(self, node);
    }

    fn visit_identifier_expression(&mut self, node: &IdentifierExpression) {
        walk_identifier_expression(self, node);
    }

    fn visit_argument_expression(&mut self, node: &ArgumentsExpression) {
        walk_argument_expression(self, node);
    }

    fn visit_unary_expression(&mut self, node: &UnaryExpression) {
        walk_unary_expression(self, node);
    }

    fn visit_assignment_expression(&mut self, node: &BinaryExpression) {
        walk_assignment_expression(self, node);
    }

    fn visit_binary_expression(&mut self, node: &BinaryExpression) {
        walk_binary_expression(self, node);
    }

    fn visit_this_expression(&mut self, node: &ThisExpression) {
        walk_this_expression(self, node);
    }

    fn visit_literal(&mut self, node: &Literal) {
        walk_literal(self, node);
    }
}

pub fn walk_program<V: Visitor>(visitor: &mut V, node: &Program) {
    for file in &node.files {
        visitor.visit_file(file)
    }
}

pub fn walk_file<V: Visitor>(visitor: &mut V, node: &File) {
    visitor.visit_source_elements(&node.source_elements);
}

pub fn walk_source_elements<V: Visitor>(visitor: &mut V, node: &SourceElements) {
    for element in &node.source_elements {
        visitor.visit_source_element(element);
    }
}

pub fn walk_source_element<V: Visitor>(visitor: &mut V, node: &SourceElement) {
    match node {
        SourceElement::FunctionDeclaration(elem) => visitor.visit_function_declaration(elem),
        SourceElement::VariableDeclaration(elem) => visitor.visit_variable_declaration(elem),
        SourceElement::ClassDeclaration(elem) => visitor.visit_class_declaration(elem),
        SourceElement::ImportDeclaration(elem) => visitor.visit_import_declaration(elem),
    }
}

pub fn walk_statement_element<V: Visitor>(visitor: &mut V, node: &StatementElement) {
    match node {
        StatementElement::Block(stmt) => visitor.visit_block_statement(stmt),
        StatementElement::Empty(stmt) => visitor.visit_empty_statement(stmt),
        StatementElement::Return(stmt) => visitor.visit_return_statement(stmt),
        StatementElement::Variable(stmt) => visitor.visit_variable_statement(stmt),
        StatementElement::Expression(stmt) => visitor.visit_expression_statement(stmt),
        StatementElement::If(stmt) => visitor.visit_if_statement(stmt),
        StatementElement::Iteration(stmt) => visitor.visit_iteration_statement(stmt),
    }
}

pub fn walk_block_statement<V: Visitor>(visitor: &mut V, node: &BlockStatement) {
    visitor.visit_statement_list(&node.statements);
}

pub fn walk_empty_statement<V: Visitor>(_visitor: &mut V, _node: &EmptyStatement) {
    // No-op
}

pub fn walk_if_statement<V: Visitor>(visitor: &mut V, node: &IfStatement) {
    visitor.visit_single_expression(&node.condition);
    visitor.visit_statement_element(&node.consequence);
    if let Some(alternative) = &node.alternative {
        visitor.visit_statement_element(&*alternative);
    }
}

pub fn walk_iteration_statement<V: Visitor>(visitor: &mut V, node: &IterationStatement) {
    match node {
        IterationStatement::While(elem) => visitor.visit_while_iteration_element(elem),
    }
}

pub fn walk_while_iteration_element<V: Visitor>(visitor: &mut V, node: &WhileIterationElement) {
    visitor.visit_single_expression(&node.expression);
    visitor.visit_block_statement(&node.block);
}

pub fn walk_return_statement<V: Visitor>(visitor: &mut V, node: &ReturnStatement) {
    visitor.visit_single_expression(&node.expression);
}

pub fn walk_variable_statement<V: Visitor>(visitor: &mut V, node: &VariableStatement) {
    visitor.visit_assignable_element(&node.target);
    visitor.visit_single_expression(&node.expression);
}

pub fn walk_expression_statement<V: Visitor>(visitor: &mut V, node: &ExpressionStatement) {
    visitor.visit_single_expression(&node.expression);
}

pub fn walk_statement_list<V: Visitor>(visitor: &mut V, node: &StatementList) {
    for statement in &node.statements {
        visitor.visit_statement_element(statement);
    }
}

pub fn walk_function_declaration<V: Visitor>(visitor: &mut V, node: &FunctionDeclarationElement) {
    visitor.visit_block_statement(&node.body);
}

pub fn walk_class_declaration<V: Visitor>(visitor: &mut V, node: &ClassDeclarationElement) {
    visitor.visit_class_body(&node.body);
}

pub fn walk_variable_declaration<V: Visitor>(visitor: &mut V, node: &VariableDeclarationElement) {
    visitor.visit_single_expression(&node.expression);
}

pub fn walk_import_declaration<V: Visitor>(_visitor: &mut V, _node: &ImportDeclarationElement) {
    // TODO
}

pub fn walk_class_body<V: Visitor>(visitor: &mut V, node: &ClassBody) {
    for class_element in &node.class_elements {
        match class_element {
            ClassElement::Constructor(elem) => visitor.visit_class_constructor_declaration(elem),
            ClassElement::Method(elem) => visitor.visit_class_method_declaration(elem),
            ClassElement::Field(elem) => visitor.visit_class_field_declaration(elem),
        }
    }
}

pub fn walk_class_constructor_declaration<V: Visitor>(
    visitor: &mut V,
    node: &ClassConstructorElement,
) {
    visitor.visit_block_statement(&node.body);
}

pub fn walk_class_method_declaration<V: Visitor>(visitor: &mut V, node: &ClassMethodElement) {
    visitor.visit_block_statement(&node.body);
}

pub fn walk_class_field_declaration<V: Visitor>(_visitor: &mut V, _node: &ClassFieldElement) {
    // TODO
}

pub fn walk_single_expression<V: Visitor>(visitor: &mut V, node: &SingleExpression) {
    match node {
        SingleExpression::Arguments(exp) => visitor.visit_argument_expression(exp),
        SingleExpression::Literal(lit) => visitor.visit_literal(lit),
        SingleExpression::Multiplicative(exp) => visitor.visit_binary_expression(exp),
        SingleExpression::Additive(exp) => visitor.visit_binary_expression(exp),
        SingleExpression::Identifier(ident) => visitor.visit_identifier_expression(ident),
        SingleExpression::Equality(exp) => visitor.visit_binary_expression(exp),
        SingleExpression::Bitwise(exp) => visitor.visit_binary_expression(exp),
        SingleExpression::Relational(exp) => visitor.visit_binary_expression(exp),
        SingleExpression::Assignment(exp) => visitor.visit_assignment_expression(exp),
        SingleExpression::Unary(exp) => visitor.visit_unary_expression(exp),
        SingleExpression::MemberIndex(exp) => visitor.visit_member_index(exp),
        SingleExpression::This(exp) => visitor.visit_this_expression(exp),
        SingleExpression::MemberDot(exp) => visitor.visit_member_dot(exp),
        SingleExpression::New(exp) => visitor.visit_new(exp),
    }
}

pub fn walk_assignable_element<V: Visitor>(_visitor: &mut V, _node: &AssignableElement) {
    // No-op
}

pub fn walk_member_dot<V: Visitor>(visitor: &mut V, node: &MemberDotExpression) {
    visitor.visit_single_expression(&node.target);
    visitor.visit_single_expression(&node.expression);
}

pub fn walk_member_index<V: Visitor>(visitor: &mut V, node: &MemberIndexExpression) {
    visitor.visit_single_expression(&node.target);
    visitor.visit_single_expression(&node.index);
}

pub fn walk_new<V: Visitor>(visitor: &mut V, node: &NewExpression) {
    visitor.visit_single_expression(&node.expression)
}

pub fn walk_identifier_expression<V: Visitor>(_visitor: &mut V, _node: &IdentifierExpression) {
    // No-op
}

pub fn walk_argument_expression<V: Visitor>(visitor: &mut V, node: &ArgumentsExpression) {
    visitor.visit_single_expression(&*node.ident);
    for arg in &node.arguments.arguments {
        visitor.visit_single_expression(arg);
    }
}

pub fn walk_unary_expression<V: Visitor>(visitor: &mut V, node: &UnaryExpression) {
    visitor.visit_single_expression(&node.expr)
}

pub fn walk_assignment_expression<V: Visitor>(visitor: &mut V, node: &BinaryExpression) {
    visitor.visit_single_expression(&node.left);
    visitor.visit_single_expression(&node.right);
}

pub fn walk_binary_expression<V: Visitor>(visitor: &mut V, node: &BinaryExpression) {
    visitor.visit_single_expression(&node.left);
    visitor.visit_single_expression(&node.right);
}

pub fn walk_this_expression<V: Visitor>(_visitor: &mut V, _node: &ThisExpression) {
    // No-op
}

pub fn walk_literal<V: Visitor>(_visitor: &mut V, _node: &Literal) {
    // No-op
}
