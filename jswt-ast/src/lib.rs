mod expression;
mod ident;
mod iteration;
mod literal;
mod statement;
mod types;
mod variable;
mod visitor;

pub mod mut_visit;
pub mod transform;
pub mod visit;

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

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub files: Vec<File>,
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct File {
    pub span: Span,
    pub source_elements: SourceElements,
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct SourceElements {
    pub span: Span,
    pub source_elements: Vec<SourceElement>,
}

#[derive(Debug, PartialEq, FromEnumVariant, Spannable, Clone)]
pub enum SourceElement {
    FunctionDeclaration(FunctionDeclarationElement),
    ClassDeclaration(ClassDeclarationElement),
    Statement(StatementElement),
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct ClassDeclarationElement {
    pub span: Span,
    pub ident: Identifier,
    pub body: ClassBody,
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct ClassBody {
    pub span: Span,
    pub class_elements: Vec<ClassElement>,
}

#[derive(Debug, PartialEq, Spannable, FromEnumVariant, Clone)]
pub enum ClassElement {
    Constructor(ClassConstructorElement),
    Field(ClassFieldElement),
    Method(ClassMethodElement),
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct ClassConstructorElement {
    pub span: Span,
    pub params: FormalParameterList,
    pub body: BlockStatement,
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct ClassMethodElement {
    pub span: Span,
    pub annotations: Vec<Annotation>,
    pub ident: Identifier,
    pub params: FormalParameterList,
    pub returns: Option<TypeAnnotation>,
    pub body: BlockStatement,
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct ClassFieldElement {
    pub span: Span,
    pub annotations: Vec<Annotation>,
    pub ident: Identifier,
    pub type_annotation: TypeAnnotation,
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct FunctionDeclarationElement {
    pub span: Span,
    pub decorators: FunctionDecorators,
    pub ident: Identifier,
    pub params: FormalParameterList,
    pub returns: Option<TypeAnnotation>,
    pub body: BlockStatement,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDecorators {
    pub annotations: Vec<Annotation>,
    pub export: bool,
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct Annotation {
    pub span: Span,
    pub name: Identifier,
    pub expr: Option<SingleExpression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FormalParameterList {
    pub parameters: Vec<FormalParameterArg>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FormalParameterArg {
    pub ident: Identifier,
    pub type_annotation: TypeAnnotation,
}

#[derive(Debug, PartialEq, Spannable, Clone)]
pub struct FunctionBody {
    pub span: Span,
    pub source_elements: SourceElements,
}

#[derive(Debug, PartialEq, Clone)]
pub struct StatementList {
    pub statements: Vec<StatementElement>,
}

#[derive(Debug, PartialEq, FromEnumVariant, Spannable, Clone)]
pub enum AssignableElement {
    Identifier(Identifier),
}
