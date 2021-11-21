use crate::ast::program::*;

macro_rules! visitor {
    ( $($fname:ident: $node:tt),*) => {
        pub trait Visitor<T> {
            $(
                fn $fname(&mut self, node: &$node) -> &mut T;
            )*
        }
    };
}

visitor![
    visit_program: Program,
    visit_source_elements: SourceElements,
    visit_source_element: SourceElement,
    visit_statement_element: StatementElement,
    visit_block_statement: BlockStatement,
    visit_empty_statement: EmptyStatement,
    visit_return_statement: ReturnStatement,
    visit_variable_statement: VariableStatement,
    visit_statement_list: StatementList,
    visit_function_declaration: FunctionDeclarationElement,
    visit_assignable_element: AssignableElement,
    visit_single_expression: SingleExpression,
    visit_literal: Literal
];
