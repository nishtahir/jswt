mod error;
mod globals;
mod locals;
mod types;

pub use error::SemanticError;
pub use globals::GlobalSemanticResolver;
pub use locals::LocalSemanticResolver;

use jswt_symbols::Symbol;
use std::borrow::Cow;

type SymbolTable = jswt_symbols::SymbolTable<Cow<'static, str>, Symbol>;
