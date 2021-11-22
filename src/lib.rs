#[macro_use]
extern crate lazy_static;

mod ast;
mod error;
// mod parser;
mod token;
mod tokenizer;

// pub mod wasm;

// pub use parser::Parser;
pub use tokenizer::Tokenizer;
