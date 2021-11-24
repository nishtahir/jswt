#[macro_use]
extern crate clap;

use clap::Arg;
// use jswt::wasm::Module;
// use jswt::wasm::Serialize;
use jswt::errors::{code_frame, location_from_offset, Location, NodeLocation, TokenizerError};
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
    let ast = Parser::new(&mut tokenizer).parse().unwrap();
    fs::write("test.ast", format!("{:#?}", ast)).unwrap();

    let mut has_errors = false;
    // Report tokenizer errors
    for error in tokenizer.errors() {
        has_errors = true;
        match error {
            TokenizerError::UnreconizedToken {
                file,
                token,
                offset,
            } => {
                let source = tokenizer.get_source(file);
                let location = location_from_offset(source, *offset);
                println!("{}:{}:{} - error", file, location.line, location.col,);

                let error_span = NodeLocation {
                    end: Location {
                        line: location.line,
                        col: location.col + 1,
                    },
                    start: location,
                };
                let frame = code_frame(
                    source,
                    error_span,
                    &format!("Unrecognized token '{}'", token),
                );

                println!("{}", frame);
            }
            TokenizerError::UnexpectedEof => todo!(),
        }
    }

    if has_errors {
        exit(1);
    }

    let mut resolver = Resolver::default();
    resolver.resolve(ast);

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
