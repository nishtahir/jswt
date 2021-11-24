#[macro_use]
extern crate lazy_static;

mod ast;
mod errors;
mod parser;
mod semantics;
mod tokenizer;

// pub mod wasm;

pub use errors::*;
pub use parser::Parser;
pub use semantics::Resolver;
pub use tokenizer::Tokenizer;
