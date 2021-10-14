#[macro_use]
extern crate clap;

use clap::Arg;
use jswt::Parser;
use jswt::Tokenizer;
use std::env;
use std::fs;

fn main() {
    let matches = app_from_crate!()
        .arg(
            Arg::new("INPUT")
                .about("Sets the input file to use")
                .required(true)
                .index(1),
        )
        .get_matches();

    if let Some(i) = matches.value_of("INPUT") {
        let content = fs::read_to_string(i).expect("Something went wrong reading the file");
        let tokens = Tokenizer::new(&content)
            .tokenize()
            .expect("Failed to tokenize");

        // TODO - probably have better logging here
        println!("{:#?}", tokens);

        let ast = Parser::new(tokens).parse().unwrap();

        println!("{:#?}", ast);
    }
}
