#[macro_use]
extern crate clap;

use clap::Arg;
// use jswt::wasm::Module;
// use jswt::wasm::Serialize;
use jswt::Parser;
use jswt::Tokenizer;
use jswt::TokenizerError;
use std::env;
use std::fs;
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
    // TODO - probably have better logging here

    // parse tokens and generate AST
    let ast = Parser::new(&mut tokenizer).parse().unwrap();
    fs::write("test.ast", format!("{:#?}", ast)).unwrap();

    // Report tokenizer errors
    for error in tokenizer.errors() {
        match error {
            TokenizerError::UnreconizedToken {
                file,
                token,
                offset,
            } => {
                let source = tokenizer.get_source(file);
                let lines: Vec<(usize, &str)> = source.split("\n").enumerate().collect();
                let (line, col) = find_error_location_in_source(&lines, *offset);
                println!(
                    "{}:{}:{} - error: Unrecognized token '{}'",
                    file, line, col, token
                );

                // Render context
                if line > 1 {
                    println!("{}", lines[line - 2].1);
                }
                println!("{}", lines[line - 1].1);

                let padding = (0..col - 1).map(|_| " ").collect::<String>();
                println!("{}{}", padding, "^");

                if line <= lines.len() - 1{
                    println!("{}\n", lines[line].1);
                }
            }
            TokenizerError::UnexpectedEof => todo!(),
        }
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

fn find_error_location_in_source(lines: &Vec<(usize, &str)>, offset: usize) -> (usize, usize) {
    if lines.is_empty() || offset == 0 {
        return (1, 1);
    }

    let mut remaining = offset + 1;
    for (i, content) in lines {
        for (col, _) in content.chars().enumerate() {
            if remaining > 0 {
                // These are not indexes
                remaining -= 1;
            }
            if remaining <= 0 {
                // These are not indexes
                return (*i + 1, col + 1);
            }
        }
        // Deduct for newline character
        if remaining > 0 {
            // These are not indexes
            remaining -= 1;
        }
    }
    unreachable!();
}

// fn native_println() {
//     println!("Hello world");
// }
