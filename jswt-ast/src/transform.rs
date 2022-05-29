use crate::*;

pub trait TransformVisitor: Sized {
    fn visit_program(&mut self, node: &Program) -> Program {
        walk_program(self, node)
    }

    fn visit_file(&mut self, node: &File) -> File {
        walk_file(self, node)
    }

    fn visit_source_elements(&mut self, node: &SourceElements) -> SourceElements {
        walk_source_elements(self, node)
    }

    fn visit_source_element(&mut self, node: &SourceElement) -> SourceElements {
        walk_source_element(self, node)
    }

    fn visit_function_declaration(&mut self, node: &FunctionDeclarationElement) -> SourceElements {
        walk_function_declaration(self, node)
    }

    fn visit_class_declaration(&mut self, node: &ClassDeclarationElement) -> SourceElements {
        walk_class_declaration(self, node)
    }

    fn visit_class_body(&mut self, node: &ClassBody) -> SourceElements {
        walk_class_body(self, node)
    }

    fn visit_class_constructor_declaration(
        &mut self,
        node: &ClassConstructorElement,
    ) -> SourceElements {
        walk_class_constructor_declaration(self, node)
    }

    fn visit_class_method_declaration(&mut self, node: &ClassMethodElement) -> SourceElements {
        walk_class_method_declaration(self, node)
    }

    fn visit_class_field_declaration(&mut self, node: &ClassFieldElement) -> SourceElements {
        walk_class_field_declaration(self, node)
    }

    fn visit_statement_element(&mut self, node: &StatementElement) -> StatementList {
        walk_statement_element(self, node)
    }

    fn visit_block_statement(&mut self, node: &BlockStatement) -> StatementList {
        walk_block_statement(self, node)
    }

    fn visit_empty_statement(&mut self, node: &EmptyStatement) -> StatementList {
        walk_empty_statement(self, node)
    }

    fn visit_if_statement(&mut self, node: &IfStatement) -> StatementList {
        walk_if_statement(self, node)
    }

    fn visit_iteration_statement(&mut self, node: &IterationStatement) -> StatementList {
        walk_iteration_statement(self, node)
    }

    fn visit_while_iteration_element(&mut self, node: &WhileIterationElement) -> StatementList {
        walk_while_iteration_element(self, node)
    }

    fn visit_return_statement(&mut self, node: &ReturnStatement) -> StatementList {
        walk_return_statement(self, node)
    }

    fn visit_variable_statement(&mut self, node: &VariableStatement) -> StatementList {
        walk_variable_statement(self, node)
    }

    fn visit_expression_statement(&mut self, node: &ExpressionStatement) -> StatementList {
        walk_expression_statement(self, node)
    }

    fn visit_statement_list(&mut self, node: &StatementList) -> StatementList {
        walk_statement_list(self, node)
    }

    fn visit_single_expression(&mut self, node: &SingleExpression) -> SingleExpression {
        walk_single_expression(self, node)
    }

    fn visit_member_dot(&mut self, node: &MemberDotExpression) -> SingleExpression {
        walk_member_dot(self, node)
    }

    fn visit_member_index(&mut self, node: &MemberIndexExpression) -> SingleExpression {
        walk_member_index(self, node)
    }

    fn visit_new(&mut self, node: &NewExpression) -> SingleExpression {
        walk_new(self, node)
    }

    fn visit_identifier_expression(&mut self, node: &IdentifierExpression) -> SingleExpression {
        walk_identifier_expression(self, node)
    }

    fn visit_argument_expression(&mut self, node: &ArgumentsExpression) -> SingleExpression {
        walk_argument_expression(self, node)
    }

    fn visit_unary_expression(&mut self, node: &UnaryExpression) -> SingleExpression {
        walk_unary_expression(self, node)
    }

    fn visit_assignment_expression(&mut self, node: &BinaryExpression) -> SingleExpression {
        walk_assignment_expression(self, node)
    }

    fn visit_binary_expression(&mut self, node: &BinaryExpression) -> SingleExpression {
        walk_binary_expression(self, node)
    }

    fn visit_this_expression(&mut self, node: &ThisExpression) -> SingleExpression {
        walk_this_expression(self, node)
    }

    fn visit_literal(&mut self, node: &Literal) -> SingleExpression {
        walk_literal(self, node)
    }

    fn visit_assignable_element(&mut self, node: &AssignableElement) -> AssignableElement {
        walk_assignable_element(self, node)
    }
}

pub fn walk_program<V: TransformVisitor>(visitor: &mut V, node: &Program) -> Program {
    let mut files = vec![];
    for file in &node.files {
        files.push(visitor.visit_file(file))
    }
    Program { files }
}

pub fn walk_file<V: TransformVisitor>(visitor: &mut V, node: &File) -> File {
    let source_elements = visitor.visit_source_elements(&node.source_elements);
    File {
        span: node.span(),
        source_elements,
    }
}

pub fn walk_source_elements<V: TransformVisitor>(
    visitor: &mut V,
    node: &SourceElements,
) -> SourceElements {
    let mut source_elements = vec![];
    for element in &node.source_elements {
        let mut elements = visitor.visit_source_element(element);
        source_elements.append(&mut elements.source_elements);
    }
    SourceElements {
        span: node.span(),
        source_elements,
    }
}

// It's possible for an element to be lowered to a list of elements
// for example a Class => Vec<Function>
pub fn walk_source_element<V: TransformVisitor>(
    visitor: &mut V,
    node: &SourceElement,
) -> SourceElements {
    match node {
        SourceElement::FunctionDeclaration(elem) => visitor.visit_function_declaration(elem),
        SourceElement::Statement(elem) => SourceElements {
            span: node.span(),
            source_elements: vec![SourceElement::Statement(StatementElement::Block(
                BlockStatement {
                    span: elem.span(),
                    statements: visitor.visit_statement_element(elem),
                },
            ))],
        },
        SourceElement::ClassDeclaration(elem) => visitor.visit_class_declaration(elem),
    }
}

pub fn walk_statement_element<V: TransformVisitor>(
    visitor: &mut V,
    node: &StatementElement,
) -> StatementList {
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

pub fn walk_block_statement<V: TransformVisitor>(
    visitor: &mut V,
    node: &BlockStatement,
) -> StatementList {
    visitor.visit_statement_list(&node.statements)
}

pub fn walk_empty_statement<V: TransformVisitor>(
    _visitor: &mut V,
    _node: &EmptyStatement,
) -> StatementList {
    StatementList { statements: vec![] }
}

pub fn walk_if_statement<V: TransformVisitor>(
    visitor: &mut V,
    node: &IfStatement,
) -> StatementList {
    StatementList {
        statements: vec![StatementElement::If(IfStatement {
            span: node.span(),
            condition: visitor.visit_single_expression(&node.condition),
            consequence: Box::new(StatementElement::Block(BlockStatement {
                span: node.consequence.span(),
                statements: visitor.visit_statement_element(&node.consequence),
            })),
            alternative: node.alternative.as_ref().map(|alt| {
                Box::new(StatementElement::Block(BlockStatement {
                    span: alt.span(),
                    statements: visitor.visit_statement_element(&*alt),
                }))
            }),
        })],
    }
}

pub fn walk_iteration_statement<V: TransformVisitor>(
    visitor: &mut V,
    node: &IterationStatement,
) -> StatementList {
    match node {
        IterationStatement::While(elem) => visitor.visit_while_iteration_element(elem),
    }
}

pub fn walk_while_iteration_element<V: TransformVisitor>(
    visitor: &mut V,
    node: &WhileIterationElement,
) -> StatementList {
    StatementList {
        statements: vec![StatementElement::Iteration(IterationStatement::While(
            WhileIterationElement {
                span: node.span(),
                expression: visitor.visit_single_expression(&node.expression),
                block: BlockStatement {
                    span: node.block.span(),
                    statements: visitor.visit_block_statement(&node.block),
                },
            },
        ))],
    }
}

pub fn walk_return_statement<V: TransformVisitor>(
    visitor: &mut V,
    node: &ReturnStatement,
) -> StatementList {
    StatementList {
        statements: vec![StatementElement::Return(ReturnStatement {
            span: node.expression.span(),
            expression: visitor.visit_single_expression(&node.expression),
        })],
    }
}

pub fn walk_variable_statement<V: TransformVisitor>(
    visitor: &mut V,
    node: &VariableStatement,
) -> StatementList {
    StatementList {
        statements: vec![StatementElement::Variable(VariableStatement {
            span: node.span(),
            modifier: node.modifier.clone(),
            target: visitor.visit_assignable_element(&node.target),
            expression: visitor.visit_single_expression(&node.expression),
            type_annotation: node.type_annotation.clone(),
        })],
    }
}

pub fn walk_expression_statement<V: TransformVisitor>(
    visitor: &mut V,
    node: &ExpressionStatement,
) -> StatementList {
    StatementList {
        statements: vec![StatementElement::Expression(ExpressionStatement {
            span: node.expression.span(),
            expression: visitor.visit_single_expression(&node.expression),
        })],
    }
}

pub fn walk_statement_list<V: TransformVisitor>(
    visitor: &mut V,
    node: &StatementList,
) -> StatementList {
    let mut statements = vec![];
    for statement in &node.statements {
        let mut inner = visitor.visit_statement_element(statement);
        statements.append(&mut inner.statements);
    }
    StatementList { statements }
}

pub fn walk_function_declaration<V: TransformVisitor>(
    visitor: &mut V,
    node: &FunctionDeclarationElement,
) -> SourceElements {
    let body = visitor.visit_block_statement(&node.body);
    let mut declaration = node.clone();
    declaration.body = BlockStatement {
        span: node.body.span(),
        statements: body,
    };

    SourceElements {
        span: node.span(),
        source_elements: vec![SourceElement::FunctionDeclaration(declaration)],
    }
}

pub fn walk_class_declaration<V: TransformVisitor>(
    visitor: &mut V,
    node: &ClassDeclarationElement,
) -> SourceElements {
    visitor.visit_class_body(&node.body)
}

pub fn walk_class_body<V: TransformVisitor>(visitor: &mut V, node: &ClassBody) -> SourceElements {
    let mut source_elements = vec![];
    for class_element in &node.class_elements {
        let mut elements = match class_element {
            ClassElement::Constructor(elem) => visitor.visit_class_constructor_declaration(elem),
            ClassElement::Method(elem) => visitor.visit_class_method_declaration(elem),
            ClassElement::Field(elem) => visitor.visit_class_field_declaration(elem),
        };
        source_elements.append(&mut elements.source_elements);
    }

    SourceElements {
        span: node.span(),
        source_elements,
    }
}

pub fn walk_class_constructor_declaration<V: TransformVisitor>(
    _visitor: &mut V,
    _node: &ClassConstructorElement,
) -> SourceElements {
    unimplemented!("This should be overriden.")
}

pub fn walk_class_method_declaration<V: TransformVisitor>(
    _visitor: &mut V,
    _node: &ClassMethodElement,
) -> SourceElements {
    unimplemented!("This should be overriden.")
}

pub fn walk_class_field_declaration<V: TransformVisitor>(
    _visitor: &mut V,
    _node: &ClassFieldElement,
) -> SourceElements {
    unimplemented!("This should be overriden.")
}

pub fn walk_single_expression<V: TransformVisitor>(
    visitor: &mut V,
    node: &SingleExpression,
) -> SingleExpression {
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

pub fn walk_assignable_element<V: TransformVisitor>(
    _visitor: &mut V,
    node: &AssignableElement,
) -> AssignableElement {
    node.clone()
}

pub fn walk_member_dot<V: TransformVisitor>(
    visitor: &mut V,
    node: &MemberDotExpression,
) -> SingleExpression {
    SingleExpression::MemberDot(MemberDotExpression {
        span: node.span(),
        target: Box::new(visitor.visit_single_expression(&node.target)),
        expression: Box::new(visitor.visit_single_expression(&node.expression)),
    })
}

pub fn walk_member_index<V: TransformVisitor>(
    visitor: &mut V,
    node: &MemberIndexExpression,
) -> SingleExpression {
    SingleExpression::MemberIndex(MemberIndexExpression {
        span: node.span(),
        target: Box::new(visitor.visit_single_expression(&node.target)),
        index: Box::new(visitor.visit_single_expression(&node.index)),
    })
}

pub fn walk_new<V: TransformVisitor>(visitor: &mut V, node: &NewExpression) -> SingleExpression {
    SingleExpression::New(NewExpression {
        span: node.span(),
        expression: Box::new(visitor.visit_single_expression(&node.expression)),
    })
}

pub fn walk_identifier_expression<V: TransformVisitor>(
    _visitor: &mut V,
    node: &IdentifierExpression,
) -> SingleExpression {
    SingleExpression::Identifier(IdentifierExpression {
        span: node.span(),
        ident: node.ident.clone(),
    })
}

pub fn walk_argument_expression<V: TransformVisitor>(
    visitor: &mut V,
    node: &ArgumentsExpression,
) -> SingleExpression {
    let argument_list = &node.arguments;
    let mut arguments = vec![];
    for arg in &argument_list.arguments {
        arguments.push(visitor.visit_single_expression(arg));
    }

    SingleExpression::Arguments(ArgumentsExpression {
        span: node.span(),
        ident: node.ident.clone(),
        arguments: ArgumentsList {
            span: argument_list.span(),
            arguments,
        },
    })
}

pub fn walk_unary_expression<V: TransformVisitor>(
    visitor: &mut V,
    node: &UnaryExpression,
) -> SingleExpression {
    SingleExpression::Unary(UnaryExpression {
        span: node.span(),
        op: node.op.clone(),
        expr: Box::new(visitor.visit_single_expression(&node.expr)),
    })
}

pub fn walk_assignment_expression<V: TransformVisitor>(
    visitor: &mut V,
    node: &BinaryExpression,
) -> SingleExpression {
    let left = Box::new(visitor.visit_single_expression(&node.left));
    let right = Box::new(visitor.visit_single_expression(&node.right));
    let op = node.op.clone();
    SingleExpression::Assignment(BinaryExpression {
        span: node.span(),
        left,
        op,
        right,
    })
}

pub fn walk_binary_expression<V: TransformVisitor>(
    visitor: &mut V,
    node: &BinaryExpression,
) -> SingleExpression {
    let span = node.span();
    let left = Box::new(visitor.visit_single_expression(&node.left));
    let right = Box::new(visitor.visit_single_expression(&node.right));
    let op = node.op.clone();
    match op {
        BinaryOperator::Plus(_) | BinaryOperator::Minus(_) => {
            SingleExpression::Additive(BinaryExpression {
                span,
                left,
                op,
                right,
            })
        }
        BinaryOperator::Mult(_) | BinaryOperator::Div(_) => {
            SingleExpression::Multiplicative(BinaryExpression {
                span,
                left,
                op,
                right,
            })
        }
        BinaryOperator::Equal(_) | BinaryOperator::NotEqual(_) => {
            SingleExpression::Equality(BinaryExpression {
                span,
                left,
                op,
                right,
            })
        }
        BinaryOperator::Greater(_)
        | BinaryOperator::GreaterEqual(_)
        | BinaryOperator::Less(_)
        | BinaryOperator::LessEqual(_) => SingleExpression::Relational(BinaryExpression {
            span,
            left,
            op,
            right,
        }),
        BinaryOperator::And(_) | BinaryOperator::Or(_) => {
            SingleExpression::Bitwise(BinaryExpression {
                span,
                left,
                op,
                right,
            })
        }
        BinaryOperator::Assign(_) => SingleExpression::Assignment(BinaryExpression {
            span,
            left,
            op,
            right,
        }),
    }
}

pub fn walk_this_expression<V: TransformVisitor>(
    _visitor: &mut V,
    node: &ThisExpression,
) -> SingleExpression {
    SingleExpression::This(node.clone())
}

pub fn walk_literal<V: TransformVisitor>(_visitor: &mut V, node: &Literal) -> SingleExpression {
    SingleExpression::Literal(node.clone())
}
