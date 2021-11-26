#[macro_use]
extern crate clap;

use clap::Arg;
// use jswt::wasm::Module;
// use jswt::wasm::Serialize;
use jswt::errors::{print_parser_error, print_semantic_error, print_tokenizer_error};
use jswt::Parser;
use jswt::Resolver;
use jswt::Tokenizer;
use std::env;
use std::fs;
use std::process::exit;
// use wasmer::Function;
// use wasmer::{imports, Instance, Module as WasmerModule, Store, Value};

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

    // Report tokenizer errors
    for error in parser_errors.1 {
        has_errors = true;
        print_tokenizer_error(&error, tokenizer.source_map())
    }

    for error in parser_errors.0 {
        has_errors = true;
        print_parser_error(&error, tokenizer.source_map());
    }

    if has_errors {
        exit(1);
    }

    let mut resolver = Resolver::default();
    resolver.resolve(&ast);

    for error in resolver.errors() {
        has_errors = true;
        print_semantic_error(&error, tokenizer.source_map())
    }

    if has_errors {
        exit(1);
    }

    // let module = Module::new(ast);
    // let serialized_wasm = module.serialize().unwrap();

    // // to test decompilation
    // // use https://webassembly.github.io/wabt/demo/wasm2wat/
    // fs::write("test.wasm", &serialized_wasm).unwrap();

    // // Embed wasmer runtime and execute generated wasm
    // let store = Store::default();
    // let module = WasmerModule::new(&store, &serialized_wasm).unwrap();
    // // The module doesn't import anything, so we create an empty import object.
    // let import_object = imports! {
    //     "env" => {
    //         "println" => Function::new_native(&store, native_println),
    //     }
    // };
    // let instance = Instance::new(&module, &import_object).unwrap();

    // let main = instance.exports.get_function("main").unwrap();
    // let result = main.call(&[Value::I32(42)]).unwrap();
    // assert_eq!(result[0], Value::I32(42));
}

// fn native_println() {
//     println!("Hello world");
// }
