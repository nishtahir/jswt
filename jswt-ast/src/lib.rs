mod ident;
mod literal;
mod single_expression;
mod statement_element;
mod visitor;

pub use crate::ident::Ident;

pub use jswt_common::{Span, Spannable};
pub use literal::*;
pub use single_expression::*;
pub use statement_element::*;
pub use visitor::*;

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
    pub source_elements: SourceElements,
}

#[derive(Debug, PartialEq)]
pub struct SourceElements {
    pub source_elements: Vec<SourceElement>,
}

#[derive(Debug, PartialEq)]
pub enum SourceElement {
    FunctionDeclaration(FunctionDeclarationElement),
    Statement(StatementElement),
}

impl From<StatementElement> for SourceElement {
    fn from(v: StatementElement) -> Self {
        Self::Statement(v)
    }
}

impl From<FunctionDeclarationElement> for SourceElement {
    fn from(v: FunctionDeclarationElement) -> Self {
        Self::FunctionDeclaration(v)
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionDeclarationElement {
    pub span: Span,
    pub decorators: FunctionDecorators,
    pub ident: Ident,
    pub params: FormalParameterList,
    pub returns: Option<Ident>,
    pub body: FunctionBody,
}

#[derive(Debug, PartialEq)]
pub struct FunctionDecorators {
    pub annotation: Option<Annotation>,
    pub export: bool,
}

#[derive(Debug, PartialEq)]
pub struct Annotation {
    pub span: Span,
    pub name: Ident,
    pub expr: SingleExpression,
}

#[derive(Debug, PartialEq)]
pub struct FormalParameterList {
    pub parameters: Vec<FormalParameterArg>,
}

#[derive(Debug, PartialEq)]
pub struct FormalParameterArg {
    pub ident: Ident,
    pub type_annotation: Ident,
}

#[derive(Debug, PartialEq)]
pub struct FunctionBody {
    pub span: Span,
    pub source_elements: SourceElements,
}

impl Spannable for FunctionBody {
    fn span(&self) -> Span {
        self.span.to_owned()
    }
}

#[derive(Debug, PartialEq)]
pub struct StatementList {
    pub statements: Vec<StatementElement>,
}

#[derive(Debug, PartialEq)]
pub enum VariableModifier {
    Let(Span),
    Const(Span),
}

impl Spannable for VariableModifier {
    fn span(&self) -> Span {
        match self {
            VariableModifier::Let(span) => span.to_owned(),
            VariableModifier::Const(span) => span.to_owned(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum AssignableElement {
    Identifier(Ident),
}

impl Spannable for AssignableElement {
    fn span(&self) -> Span {
        match self {
            AssignableElement::Identifier(ident) => ident.span.to_owned(),
        }
    }
}

impl From<Ident> for AssignableElement {
    fn from(v: Ident) -> Self {
        Self::Identifier(v)
    }
}
