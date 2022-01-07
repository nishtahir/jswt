use std::borrow::Cow;

use jswt_types::Type;

#[derive(Debug, PartialEq)]
pub struct Symbol {
    pub ty: Type,
    pub name: Cow<'static, str>,
}

impl Symbol {
    pub fn new(ty: Type, name: Cow<'static, str>) -> Self {
        Self { ty, name }
    }
}
