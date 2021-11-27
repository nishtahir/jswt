#[macro_use]
extern crate clap;

use clap::Arg;
use jswt::codegen::CodeGenerator;
use jswt::errors::{print_parser_error, print_semantic_error, print_tokenizer_error};
use jswt::Parser;
use jswt::Resolver;
use jswt::Tokenizer;
use std::env;
use std::fs;
use std::process::exit;
use wasmer::{imports, Function, Instance, Module as WasmerModule, Store};

fn main() {
    let matches = app_from_crate!()
        .arg(
            Arg::with_name("INPUT")
                .help("Sets the input file to use")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::with_name("tokenize")
                .short("t")
                .help("Run the tokenizer"),
        )
        .get_matches();

    let i = match matches.value_of("INPUT") {
        Some(it) => it,
        _ => return,
    };

    let mut tokenizer = Tokenizer::default();
    tokenizer.push_source(i);

    if matches.is_present("tokenize") {
        let tokens = tokenizer.tokenize();
        fs::write("test.tokens", format!("{:#?}", tokens)).unwrap();
        return;
    }

    // parse tokens and generate AST
    let mut parser = Parser::new(&mut tokenizer);

    let ast = parser.parse();
    fs::write("test.ast", format!("{:#?}", ast)).unwrap();

    let mut has_errors = false;
    let parser_errors = parser.errors();

    // Report errors
    for error in parser_errors.1 {
        has_errors = true;
        print_tokenizer_error(&error, tokenizer.source_map())
    }

    for error in parser_errors.0 {
        has_errors = true;
        print_parser_error(&error, tokenizer.source_map());
    }

    // Semantic analytis pass
    let mut resolver = Resolver::default();
    resolver.resolve(&ast);

    for error in resolver.errors() {
        has_errors = true;
        print_semantic_error(&error, tokenizer.source_map())
    }

    if has_errors {
        exit(1);
    }

    let mut code_gen = CodeGenerator::default();
    let module = code_gen.generate_module(&ast);
    let wast = module.as_wat();
    fs::write("test.wast", &wast).unwrap();

    // Embed wasmer runtime and execute generated wasm
    let store = Store::default();
    let module = WasmerModule::new(&store, wast).unwrap();
    let import_object = imports! {
        "env" => {
            "println" => Function::new_native(&store, env_println)
        },
    };
    let instance = Instance::new(&module, &import_object).unwrap();

    // Assume main is a function that accepts no args and returns an i32
    // The returned i32 is the exit code
    // function main(): i32 { return 0; } // OK
    let main = instance.exports.get_function("main").unwrap();
    let result = main.call(&[]).unwrap();
    if result.len() > 0 {
        let exit_code = result[0].i32().unwrap_or(1);
        exit(exit_code);
    }
    // No exit code will panic during the validation but for safety
    // if no exit code was given assume there was a problem
    exit(1);
}

fn env_println(arg: i32) {
    println!("{}", arg);
}
