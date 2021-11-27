use super::{
    ident::Ident,
    span::{Span, Spannable},
};

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
    pub export: bool,
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
pub enum StatementElement {
    Block(BlockStatement),
    Empty(EmptyStatement),
    Return(ReturnStatement),
    Variable(VariableStatement),
    Expression(ExpressionStatement),
}

impl From<ExpressionStatement> for StatementElement {
    fn from(v: ExpressionStatement) -> Self {
        Self::Expression(v)
    }
}

impl From<ReturnStatement> for StatementElement {
    fn from(v: ReturnStatement) -> Self {
        Self::Return(v)
    }
}

impl From<VariableStatement> for StatementElement {
    fn from(v: VariableStatement) -> Self {
        Self::Variable(v)
    }
}

impl From<EmptyStatement> for StatementElement {
    fn from(v: EmptyStatement) -> Self {
        Self::Empty(v)
    }
}

impl From<BlockStatement> for StatementElement {
    fn from(v: BlockStatement) -> Self {
        Self::Block(v)
    }
}

#[derive(Debug, PartialEq)]
pub struct BlockStatement {
    pub span: Span,
    pub statements: StatementList,
}

#[derive(Debug, PartialEq)]
pub struct EmptyStatement {
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    pub span: Span,
    pub expression: SingleExpression,
}

#[derive(Debug, PartialEq)]
pub struct VariableStatement {
    pub span: Span,
    pub modifier: VariableModifier,
    pub target: AssignableElement,
    pub expression: SingleExpression,
}

#[derive(Debug, PartialEq)]
pub struct ExpressionStatement {
    pub span: Span,
    pub expression: SingleExpression,
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

#[derive(Debug, PartialEq)]
pub enum SingleExpression {
    Arguments(ArgumentsExpression),
    Multiplicative(BinaryExpression),
    Additive(BinaryExpression),
    Identifier(IdentifierExpression),
    Literal(Literal),
}

impl From<ArgumentsExpression> for SingleExpression {
    fn from(v: ArgumentsExpression) -> Self {
        Self::Arguments(v)
    }
}

impl From<IdentifierExpression> for SingleExpression {
    fn from(v: IdentifierExpression) -> Self {
        Self::Identifier(v)
    }
}

impl From<Literal> for SingleExpression {
    fn from(v: Literal) -> Self {
        Self::Literal(v)
    }
}

impl From<BinaryExpression> for SingleExpression {
    fn from(v: BinaryExpression) -> Self {
        Self::Additive(v)
    }
}

impl Spannable for SingleExpression {
    fn span(&self) -> Span {
        match self {
            SingleExpression::Multiplicative(exp) => exp.span(),
            SingleExpression::Additive(exp) => exp.span(),
            SingleExpression::Literal(exp) => exp.span(),
            SingleExpression::Identifier(exp) => exp.span(),
            SingleExpression::Arguments(exp) => exp.span(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ArgumentsExpression {
    pub span: Span,
    pub ident: Box<SingleExpression>,
    pub arguments: ArgumentsList,
}

impl Spannable for ArgumentsExpression {
    fn span(&self) -> Span {
        self.span.to_owned()
    }
}

#[derive(Debug, PartialEq)]
pub struct ArgumentsList {
    pub span: Span,
    pub arguments: Vec<SingleExpression>,
}

impl Spannable for ArgumentsList {
    fn span(&self) -> Span {
        self.span.to_owned()
    }
}

#[derive(Debug, PartialEq)]
pub struct UnaryExpression {
    pub span: Span,
    op: UnaryOperator,
    expr: Box<SingleExpression>,
}

#[derive(Debug, PartialEq)]
pub struct BinaryExpression {
    pub span: Span,
    pub left: Box<SingleExpression>,
    pub op: BinaryOperator,
    pub right: Box<SingleExpression>,
}

impl Spannable for BinaryExpression {
    fn span(&self) -> Span {
        self.span.to_owned()
    }
}

#[derive(Debug, PartialEq)]
pub struct IdentifierExpression {
    pub ident: Ident,
}

#[derive(Debug, PartialEq)]
pub enum UnaryOperator {
    Plus(Span),
    Minus(Span),
}

#[derive(Debug, PartialEq)]
pub enum BinaryOperator {
    Plus(Span),
    Minus(Span),
    Star(Span),
    Slash(Span),
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    String(StringLiteral),
    Number(NumberLiteral),
    Boolean(BooleanLiteral),
}

impl Spannable for Literal {
    fn span(&self) -> Span {
        match self {
            Literal::String(s) => s.span.to_owned(),
            Literal::Number(n) => n.span.to_owned(),
            Literal::Boolean(b) => b.span.to_owned(),
        }
    }
}

impl From<StringLiteral> for Literal {
    fn from(v: StringLiteral) -> Self {
        Self::String(v)
    }
}

impl From<NumberLiteral> for Literal {
    fn from(v: NumberLiteral) -> Self {
        Self::Number(v)
    }
}

impl From<BooleanLiteral> for Literal {
    fn from(v: BooleanLiteral) -> Self {
        Self::Boolean(v)
    }
}

#[derive(Debug, PartialEq)]
pub struct BooleanLiteral {
    pub span: Span,
    pub value: bool,
}

#[derive(Debug, PartialEq)]

pub struct NumberLiteral {
    pub span: Span,
    pub value: i32,
}

#[derive(Debug, PartialEq)]
pub struct StringLiteral {
    pub span: Span,
    pub value: &'static str,
}
impl Spannable for IdentifierExpression {
    fn span(&self) -> Span {
        self.ident.span.to_owned()
    }
}
