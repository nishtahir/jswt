#[macro_use]
extern crate jswt_derive;

pub mod transform;
mod visitor;

pub use crate::visitor::*;
pub use ast::*;
use jswt_common::{Span, Spannable, Type};
use jswt_derive::{derive_visitors, Visitor};
use std::borrow::Cow;

#[derive(Debug, PartialEq, Clone)]
pub enum VariableModifier {
    Let(Span),
    Const(Span),
}

#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOperator {
    Plus(Span),
    Minus(Span),
    Not(Span),
    PostIncrement(Span),
    PostDecrement(Span),
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOperator {
    Plus(Span),
    Minus(Span),
    Mult(Span),
    Div(Span),
    Equal(Span),
    NotEqual(Span),
    Greater(Span),
    GreaterEqual(Span),
    Less(Span),
    LessEqual(Span),
    And(Span),
    Or(Span),
    Assign(Span),
}

derive_visitors! {
    mod ast {
        use super::*;

        #[derive(Debug, PartialEq, Clone)]
        pub struct Program {
            #[walk]
            pub files: Vec<File>,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct File {
            pub span: Span,
            #[walk]
            pub source_elements: SourceElements,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct SourceElements {
            pub span: Span,
            #[walk]
            pub source_elements: Vec<SourceElement>,
        }

        #[derive(Debug, PartialEq, FromEnumVariant, Spannable, Clone)]
        pub enum SourceElement {
            FunctionDeclaration(FunctionDeclarationElement),
            ClassDeclaration(ClassDeclarationElement),
            VariableDeclaration(VariableDeclarationElement),
            ImportDeclaration(ImportDeclarationElement),
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct ClassDeclarationElement {
            pub span: Span,
            pub export: bool,
            #[walk]
            pub annotations: Vec<Annotation>,
            #[walk]
            pub ident: Identifier,
            #[walk]
            pub body: ClassBody,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct ClassBody {
            pub span: Span,
            #[walk]
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
            #[walk]
            pub params: FormalParameterList,
            #[walk]
            pub body: BlockStatement,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct ClassMethodElement {
            pub span: Span,
            #[walk]
            pub annotations: Vec<Annotation>,
            #[walk]
            pub ident: Identifier,
            #[walk]
            pub params: FormalParameterList,
            #[walk]
            pub returns: Option<TypeAnnotation>,
            #[walk]
            pub body: BlockStatement,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct ClassFieldElement {
            pub span: Span,
            #[walk]
            pub annotations: Vec<Annotation>,
            #[walk]
            pub ident: Identifier,
            #[walk]
            pub type_annotation: TypeAnnotation,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct FunctionDeclarationElement {
            pub span: Span,
            #[walk]
            pub decorators: FunctionDecorators,
            #[walk]
            pub ident: Identifier,
            #[walk]
            pub params: FormalParameterList,
            #[walk]
            pub returns: Option<TypeAnnotation>,
            #[walk]
            pub body: BlockStatement,
        }

        #[derive(Debug, PartialEq, Clone)]
        pub struct FunctionDecorators {
            #[walk]
            pub annotations: Vec<Annotation>,
            pub export: bool,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct Annotation {
            pub span: Span,
            #[walk]
            pub name: Identifier,
            #[walk]
            pub expr: Option<SingleExpression>,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct FormalParameterList {
            pub span: Span,
            #[walk]
            pub parameters: Vec<FormalParameterArg>,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct FormalParameterArg {
            pub span: Span,
            #[walk]
            pub ident: Identifier,
            #[walk]
            pub type_annotation: TypeAnnotation,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct FunctionBody {
            pub span: Span,
            #[walk]
            pub source_elements: SourceElements,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct VariableDeclarationElement {
            pub span: Span,
            #[walk]
            pub annotations: Vec<Annotation>,
            pub export: bool,
            pub modifier: VariableModifier,
            #[walk]
            pub name: Identifier,
            #[walk]
            pub expression: SingleExpression,
            #[walk]
            pub type_annotation: Option<TypeAnnotation>,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct ImportDeclarationElement {
            pub span: Span,
            pub path: Cow<'static, str>,
        }

        #[derive(Debug, PartialEq, Clone)]
        pub struct StatementList {
            #[walk]
            pub statements: Vec<StatementElement>,
        }

        #[derive(Debug, PartialEq, FromEnumVariant, Spannable, Clone)]
        pub enum AssignableElement {
            Identifier(Identifier),
        }


        #[derive(Debug, PartialEq, FromEnumVariant, Spannable, Clone)]
        pub enum IterationStatement {
            While(WhileIterationElement),
            For(ForIterationElement),
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct WhileIterationElement {
            pub span: Span,
            #[walk]
            pub expression: SingleExpression,
            #[walk]
            pub block: BlockStatement,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct ForIterationElement {
            pub span: Span,
            #[walk]
            pub initializer: SingleExpression,
            #[walk]
            pub condition: SingleExpression,
            #[walk]
            pub update: SingleExpression,
            #[walk]
            pub block: BlockStatement,
        }

        #[derive(Debug, PartialEq, FromEnumVariant, Spannable, Clone)]
        pub enum Literal {
            Array(ArrayLiteral),
            String(StringLiteral),
            Integer(IntegerLiteral),
            Float(FloatingPointLiteral),
            Boolean(BooleanLiteral),
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct BooleanLiteral {
            pub span: Span,
            pub value: bool,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct IntegerLiteral {
            pub span: Span,
            pub value: i32,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct FloatingPointLiteral {
            pub span: Span,
            pub value: f32,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct StringLiteral {
            pub span: Span,
            pub value: &'static str,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct ArrayLiteral {
            pub span: Span,
            #[walk]
            pub elements: Vec<SingleExpression>,
        }

        #[derive(Debug, PartialEq, FromEnumVariant, Spannable, Clone)]
        pub enum StatementElement {
            Block(BlockStatement),
            Empty(EmptyStatement),
            If(IfStatement),
            Iteration(IterationStatement),
            Return(ReturnStatement),
            Variable(VariableStatement),
            Expression(ExpressionStatement),
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct BlockStatement {
            pub span: Span,
            #[walk]
            pub statements: StatementList,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct EmptyStatement {
            pub span: Span,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct IfStatement {
            pub span: Span,
            #[walk]
            pub condition: SingleExpression,
            #[walk]
            pub consequence: Box<StatementElement>,
            #[walk]
            pub alternative: Option<Box<StatementElement>>,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct ReturnStatement {
            pub span: Span,
            #[walk]
            pub expression: SingleExpression,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct VariableStatement {
            pub span: Span,
            pub modifier: VariableModifier,
            #[walk]
            pub target: AssignableElement,
            #[walk]
            pub expression: SingleExpression,
            #[walk]
            pub type_annotation: Option<TypeAnnotation>,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct ExpressionStatement {
            pub span: Span,
            #[walk]
            pub expression: SingleExpression,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub enum SingleExpression {
            Unary(UnaryExpression),
            MemberIndex(MemberIndexExpression),
            New(NewExpression),
            Arguments(ArgumentsExpression),
            Assignment(AssignmentExpression),
            Multiplicative(BinaryExpression),
            Bitwise(BinaryExpression),
            Additive(BinaryExpression),
            Equality(BinaryExpression),
            Relational(BinaryExpression),
            Identifier(IdentifierExpression),
            MemberDot(MemberDotExpression),
            This(ThisExpression),
            Literal(Literal),
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct AssignmentExpression {
            pub span: Span,
            #[walk]
            pub target: Box<SingleExpression>,
            #[walk]
            pub expression: Box<SingleExpression>,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct NewExpression {
            pub span: Span,
            #[walk]
            pub expression: Box<SingleExpression>,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct MemberDotExpression {
            pub span: Span,
            #[walk]
            pub target: Box<SingleExpression>,
            #[walk]
            pub expression: Box<SingleExpression>,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct ArgumentsExpression {
            pub span: Span,
            #[walk]
            pub ident: Box<SingleExpression>,
            #[walk]
            pub arguments: ArgumentsList,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct ArgumentsList {
            pub span: Span,
            #[walk]
            pub arguments: Vec<SingleExpression>,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct ThisExpression {
            pub span: Span,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct MemberIndexExpression {
            pub span: Span,
            #[walk]
            pub target: Box<SingleExpression>,
            #[walk]
            pub index: Box<SingleExpression>,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct UnaryExpression {
            pub span: Span,
            pub op: UnaryOperator,
            #[walk]
            pub expr: Box<SingleExpression>,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct BinaryExpression {
            pub span: Span,
            #[walk]
            pub left: Box<SingleExpression>,
            pub op: BinaryOperator,
            #[walk]
            pub right: Box<SingleExpression>,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct IdentifierExpression {
            pub span: Span,
            #[walk]
            pub ident: Identifier,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct TypeAnnotation {
            pub span: Span,
            pub ty: Type,
        }

        #[derive(Debug, PartialEq, Spannable, Clone)]
        pub struct Identifier {
            pub span: Span,
            pub value: Cow<'static, str>,
        }

    }
}

#[derive(Debug)]
pub struct Ast {
    pub program: Program,
}

impl Ast {
    pub fn new(program: Program) -> Self {
        Self { program }
    }
}

impl Identifier {
    pub fn new<T: Into<Cow<'static, str>>>(value: T, span: Span) -> Self {
        Self {
            span,
            value: value.into(),
        }
    }
}

impl Spannable for VariableModifier {
    fn span(&self) -> Span {
        match self {
            VariableModifier::Let(span) => span.to_owned(),
            VariableModifier::Const(span) => span.to_owned(),
        }
    }
}

impl VariableModifier {
    pub fn is_const(&self) -> bool {
        match self {
            VariableModifier::Const(_) => true,
            _ => false,
        }
    }
}

impl SingleExpression {
    pub fn as_arguments(&self) -> Option<&ArgumentsExpression> {
        if let Self::Arguments(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_identifier(&self) -> Option<&IdentifierExpression> {
        if let Self::Identifier(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_identifier_mut(&mut self) -> Option<&mut IdentifierExpression> {
        if let Self::Identifier(v) = self {
            Some(v)
        } else {
            None
        }
    }
}
