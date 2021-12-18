use crate::high_level::*;

macro_rules! statement_visitor {
    ( $($fname:ident: $node:tt),*) => {
        pub trait StatementVisitor {
            $(
                fn $fname(&mut self, node: &$node);
            )*
        }

        pub trait MutStatementVisitor {
            $(
                fn $fname(&mut self, node: &mut $node);
            )*
        }
    };
}

macro_rules! expression_visitor {
    ( $($fname:ident: $node:tt),*) => {
        pub trait ExpressionVisitor<T> {
            $(
                fn $fname(&mut self, node: &$node)-> T;
            )*
        }

        pub trait MutExpressionVisitor<T> {
            $(
                fn $fname(&mut self, node: &mut $node)-> T;
            )*
        }
    };
}

statement_visitor![
    visit_program: Program,
    visit_source_elements: SourceElements,
    visit_source_element: SourceElement,
    visit_statement_element: StatementElement,
    visit_block_statement: BlockStatement,
    visit_empty_statement: EmptyStatement,
    visit_if_statement: IfStatement,
    visit_iteration_statement: IterationStatement,
    visit_while_iteration_element: WhileIterationElement,
    visit_return_statement: ReturnStatement,
    visit_variable_statement: VariableStatement,
    visit_expression_statement: ExpressionStatement,
    visit_statement_list: StatementList,
    visit_function_declaration: FunctionDeclarationElement,
    visit_function_body: FunctionBody
];

expression_visitor![
    visit_single_expression: SingleExpression,
    visit_assignable_element: AssignableElement,
    visit_member_index: MemberIndexExpression,
    visit_identifier_expression: IdentifierExpression,
    visit_argument_expression: ArgumentsExpression,
    visit_unary_expression: UnaryExpression,
    visit_assignment_expression: BinaryExpression,
    visit_binary_expression: BinaryExpression,
    visit_literal: Literal
];
