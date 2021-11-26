#[macro_use]
extern crate lazy_static;

pub mod assert;
mod ast;
pub mod errors;
mod parser;
mod semantics;
mod tokenizer;

// pub mod wasm;

pub use parser::Parser;
pub use semantics::Resolver;
pub use tokenizer::Tokenizer;
