use crate::*;

pub trait MutVisitor: Sized {
    fn visit_program(&mut self, node: &mut Program) {
        walk_program(self, node);
    }

    fn visit_file(&mut self, node: &mut File) {
        walk_file(self, node);
    }

    fn visit_source_elements(&mut self, node: &mut SourceElements) {
        walk_source_elements(self, node);
    }

    fn visit_source_element(&mut self, node: &mut SourceElement) {
        walk_source_element(self, node);
    }

    fn visit_statement_element(&mut self, node: &mut StatementElement) {
        walk_statement_element(self, node);
    }

    fn visit_block_statement(&mut self, node: &mut BlockStatement) {
        walk_block_statement(self, node);
    }

    fn visit_empty_statement(&mut self, node: &mut EmptyStatement) {
        walk_empty_statement(self, node);
    }

    fn visit_if_statement(&mut self, node: &mut IfStatement) {
        walk_if_statement(self, node);
    }

    fn visit_iteration_statement(&mut self, node: &mut IterationStatement) {
        walk_iteration_statement(self, node);
    }

    fn visit_while_iteration_element(&mut self, node: &mut WhileIterationElement) {
        walk_while_iteration_element(self, node);
    }

    fn visit_return_statement(&mut self, node: &mut ReturnStatement) {
        walk_return_statement(self, node);
    }

    fn visit_variable_statement(&mut self, node: &mut VariableStatement) {
        walk_variable_statement(self, node);
    }

    fn visit_expression_statement(&mut self, node: &mut ExpressionStatement) {
        walk_expression_statement(self, node);
    }

    fn visit_statement_list(&mut self, node: &mut StatementList) {
        walk_statement_list(self, node);
    }

    fn visit_function_declaration(&mut self, node: &mut FunctionDeclarationElement) {
        walk_function_declaration(self, node);
    }

    fn visit_class_declaration(&mut self, node: &mut ClassDeclarationElement) {
        walk_class_declaration(self, node);
    }

    fn visit_class_body(&mut self, node: &mut ClassBody) {
        walk_class_body(self, node);
    }

    fn visit_class_constructor_declaration(&mut self, node: &mut ClassConstructorElement) {
        walk_class_constructor_declaration(self, node);
    }

    fn visit_class_method_declaration(&mut self, node: &mut ClassMethodElement) {
        walk_class_method_declaration(self, node);
    }

    fn visit_class_field_declaration(&mut self, node: &mut ClassFieldElement) {
        walk_class_field_declaration(self, node);
    }

    fn visit_single_expression(&mut self, node: &mut SingleExpression) {
        walk_single_expression(self, node);
    }

    fn visit_assignable_element(&mut self, node: &mut AssignableElement) {
        walk_assignable_element(self, node);
    }

    fn visit_member_dot(&mut self, node: &mut MemberDotExpression) {
        walk_member_dot(self, node);
    }

    fn visit_member_index(&mut self, node: &mut MemberIndexExpression) {
        walk_member_index(self, node);
    }

    fn visit_new(&mut self, node: &mut NewExpression) {
        walk_new(self, node);
    }

    fn visit_identifier_expression(&mut self, node: &mut IdentifierExpression) {
        walk_identifier_expression(self, node);
    }

    fn visit_argument_expression(&mut self, node: &mut ArgumentsExpression) {
        walk_argument_expression(self, node);
    }

    fn visit_unary_expression(&mut self, node: &mut UnaryExpression) {
        walk_unary_expression(self, node);
    }

    fn visit_assignment_expression(&mut self, node: &mut BinaryExpression) {
        walk_assignment_expression(self, node);
    }

    fn visit_binary_expression(&mut self, node: &mut BinaryExpression) {
        walk_binary_expression(self, node);
    }

    fn visit_this_expression(&mut self, node: &mut ThisExpression) {
        walk_this_expression(self, node);
    }

    fn visit_literal(&mut self, node: &mut Literal) {
        walk_literal(self, node);
    }
}

pub fn walk_program<V: MutVisitor>(visitor: &mut V, node: &mut Program) {
    for file in &mut node.files {
        visitor.visit_file(file)
    }
}

pub fn walk_file<V: MutVisitor>(visitor: &mut V, node: &mut File) {
    visitor.visit_source_elements(&mut node.source_elements);
}

pub fn walk_source_elements<V: MutVisitor>(visitor: &mut V, node: &mut SourceElements) {
    for element in &mut node.source_elements {
        visitor.visit_source_element(element);
    }
}

pub fn walk_source_element<V: MutVisitor>(visitor: &mut V, node: &mut SourceElement) {
    match node {
        SourceElement::FunctionDeclaration(elem) => visitor.visit_function_declaration(elem),
        SourceElement::Statement(elem) => visitor.visit_statement_element(elem),
        SourceElement::ClassDeclaration(elem) => visitor.visit_class_declaration(elem),
    }
}

pub fn walk_statement_element<V: MutVisitor>(visitor: &mut V, node: &mut StatementElement) {
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

pub fn walk_block_statement<V: MutVisitor>(visitor: &mut V, node: &mut BlockStatement) {
    visitor.visit_statement_list(&mut node.statements);
}

pub fn walk_empty_statement<V: MutVisitor>(_visitor: &mut V, _node: &mut EmptyStatement) {
    // No-op
}

pub fn walk_if_statement<V: MutVisitor>(visitor: &mut V, node: &mut IfStatement) {
    visitor.visit_single_expression(&mut node.condition);
    visitor.visit_statement_element(&mut node.consequence);
    if let Some(alternative) = &mut node.alternative {
        visitor.visit_statement_element(&mut *alternative);
    }
}

pub fn walk_iteration_statement<V: MutVisitor>(visitor: &mut V, node: &mut IterationStatement) {
    match node {
        IterationStatement::While(elem) => visitor.visit_while_iteration_element(elem),
    }
}

pub fn walk_while_iteration_element<V: MutVisitor>(
    visitor: &mut V,
    node: &mut WhileIterationElement,
) {
    visitor.visit_single_expression(&mut node.expression);
    visitor.visit_block_statement(&mut node.block);
}

pub fn walk_return_statement<V: MutVisitor>(visitor: &mut V, node: &mut ReturnStatement) {
    visitor.visit_single_expression(&mut node.expression);
}

pub fn walk_variable_statement<V: MutVisitor>(visitor: &mut V, node: &mut VariableStatement) {
    visitor.visit_assignable_element(&mut node.target);
    visitor.visit_single_expression(&mut node.expression)
}

pub fn walk_expression_statement<V: MutVisitor>(visitor: &mut V, node: &mut ExpressionStatement) {
    visitor.visit_single_expression(&mut node.expression);
}

pub fn walk_statement_list<V: MutVisitor>(visitor: &mut V, node: &mut StatementList) {
    for statement in &mut node.statements {
        visitor.visit_statement_element(statement);
    }
}

pub fn walk_function_declaration<V: MutVisitor>(
    visitor: &mut V,
    node: &mut FunctionDeclarationElement,
) {
    visitor.visit_block_statement(&mut node.body);
}

pub fn walk_class_declaration<V: MutVisitor>(visitor: &mut V, node: &mut ClassDeclarationElement) {
    visitor.visit_class_body(&mut node.body);
}

pub fn walk_class_body<V: MutVisitor>(visitor: &mut V, node: &mut ClassBody) {
    for class_element in &mut node.class_elements {
        match class_element {
            ClassElement::Constructor(elem) => visitor.visit_class_constructor_declaration(elem),
            ClassElement::Method(elem) => visitor.visit_class_method_declaration(elem),
            ClassElement::Field(elem) => visitor.visit_class_field_declaration(elem),
        }
    }
}

pub fn walk_class_constructor_declaration<V: MutVisitor>(
    visitor: &mut V,
    node: &mut ClassConstructorElement,
) {
    visitor.visit_block_statement(&mut node.body);
}

pub fn walk_class_method_declaration<V: MutVisitor>(
    visitor: &mut V,
    node: &mut ClassMethodElement,
) {
    visitor.visit_block_statement(&mut node.body);
}

pub fn walk_class_field_declaration<V: MutVisitor>(
    _visitor: &mut V,
    _node: &mut ClassFieldElement,
) {
    // TODO
}

pub fn walk_single_expression<V: MutVisitor>(visitor: &mut V, node: &mut SingleExpression) {
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

pub fn walk_assignable_element<V: MutVisitor>(_visitor: &mut V, _node: &mut AssignableElement) {
    // No-op
}

pub fn walk_member_dot<V: MutVisitor>(visitor: &mut V, node: &mut MemberDotExpression) {
    visitor.visit_single_expression(&mut node.target);
    visitor.visit_single_expression(&mut node.expression);
}

pub fn walk_member_index<V: MutVisitor>(visitor: &mut V, node: &mut MemberIndexExpression) {
    visitor.visit_single_expression(&mut node.target);
    visitor.visit_single_expression(&mut node.index);
}

pub fn walk_new<V: MutVisitor>(visitor: &mut V, node: &mut NewExpression) {
    visitor.visit_single_expression(&mut node.expression)
}

pub fn walk_identifier_expression<V: MutVisitor>(
    _visitor: &mut V,
    _node: &mut IdentifierExpression,
) {
    // No-op
}

pub fn walk_argument_expression<V: MutVisitor>(visitor: &mut V, node: &mut ArgumentsExpression) {
    visitor.visit_single_expression(&mut *node.ident);
    for arg in &mut node.arguments.arguments {
        visitor.visit_single_expression(arg);
    }
}

pub fn walk_unary_expression<V: MutVisitor>(visitor: &mut V, node: &mut UnaryExpression) {
    visitor.visit_single_expression(&mut node.expr)
}

pub fn walk_assignment_expression<V: MutVisitor>(visitor: &mut V, node: &mut BinaryExpression) {
    visitor.visit_single_expression(&mut node.left);
    visitor.visit_single_expression(&mut node.right);
}

pub fn walk_binary_expression<V: MutVisitor>(visitor: &mut V, node: &mut BinaryExpression) {
    visitor.visit_single_expression(&mut node.left);
    visitor.visit_single_expression(&mut node.right);
}

pub fn walk_this_expression<V: MutVisitor>(_visitor: &mut V, _node: &mut ThisExpression) {
    // No-op
}

pub fn walk_literal<V: MutVisitor>(_visitor: &mut V, _node: &mut Literal) {
    // No-op
}
