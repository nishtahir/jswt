use std::borrow::Cow;

trait SizedType {
    fn size(&self) -> usize;
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Type {
    Binding(Cow<'static, str>),
    Unknown,
}

pub trait Typeable {
    fn ty(&self) -> Type;
    fn binding(&self) -> Option<Cow<'static, str>>;
}

impl ToString for Type {
    fn to_string(&self) -> String {
        match self {
            Type::Binding(inner) => inner.to_string(),
            Type::Unknown => "Unknown".to_string(),
        }
    }
}
