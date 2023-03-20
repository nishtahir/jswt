use jswt_common::{Span, Spannable};

#[derive(Debug, PartialEq, Clone)]
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

impl VariableModifier {
    pub fn is_const(&self) -> bool {
        match self {
            VariableModifier::Const(_) => true,
            _ => false,
        }
    }
}
