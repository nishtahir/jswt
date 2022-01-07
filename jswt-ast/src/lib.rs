mod expression;
mod ident;
mod iteration;
mod literal;
mod statement;
mod types;
mod variable;
mod visitor;

pub use expression::*;
pub use ident::*;
pub use iteration::*;
pub use literal::*;
pub use statement::*;
pub use types::*;
pub use variable::*;
pub use visitor::*;

use jswt_common::{Span, Spannable};
use jswt_derive::{FromEnumVariant, Spannable};

#[derive(Debug)]
pub struct Ast {
    pub program: Program,
}

impl Ast {
    pub fn new(program: Program) -> Self {
        Self { program }
    }
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub files: Vec<File>,
}

#[derive(Debug, PartialEq)]
pub struct File {
    pub source_elements: SourceElements,
}

#[derive(Debug, PartialEq)]
pub struct SourceElements {
    pub source_elements: Vec<SourceElement>,
}

#[derive(Debug, PartialEq, FromEnumVariant)]
pub enum SourceElement {
    FunctionDeclaration(FunctionDeclarationElement),
    ClassDeclaration(ClassDeclarationElement),
    Statement(StatementElement),
}

#[derive(Debug, PartialEq, Spannable)]
pub struct ClassDeclarationElement {
    pub span: Span,
    pub ident: Identifier,
}

#[derive(Debug, PartialEq, Spannable)]
pub struct FunctionDeclarationElement {
    pub span: Span,
    pub decorators: FunctionDecorators,
    pub ident: Identifier,
    pub params: FormalParameterList,
    pub returns: Option<TypeAnnotation>,
    pub body: FunctionBody,
}

#[derive(Debug, PartialEq)]
pub struct FunctionDecorators {
    pub annotations: Vec<Annotation>,
    pub export: bool,
}

#[derive(Debug, PartialEq, Spannable)]
pub struct Annotation {
    pub span: Span,
    pub name: Identifier,
    pub expr: Option<SingleExpression>,
}

#[derive(Debug, PartialEq)]
pub struct FormalParameterList {
    pub parameters: Vec<FormalParameterArg>,
}

#[derive(Debug, PartialEq)]
pub struct FormalParameterArg {
    pub ident: Identifier,
    pub type_annotation: TypeAnnotation,
}

#[derive(Debug, PartialEq, Spannable)]
pub struct FunctionBody {
    pub span: Span,
    pub source_elements: SourceElements,
}

#[derive(Debug, PartialEq)]
pub struct StatementList {
    pub statements: Vec<StatementElement>,
}

#[derive(Debug, PartialEq, FromEnumVariant, Spannable)]
pub enum AssignableElement {
    Identifier(Identifier),
}
