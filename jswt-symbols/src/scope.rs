use super::symbol::Symbol;
use std::{borrow::Cow, collections::BTreeMap};

#[derive(Debug, Default)]
pub struct Scope {
    pub symbols: BTreeMap<Cow<'static, str>, Symbol>,
}
