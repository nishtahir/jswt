#[macro_use]
extern crate clap;

use clap::Arg;
// use jswt::wasm::Module;
// use jswt::wasm::Serialize;
use jswt::Parser;
use jswt::Tokenizer;
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
        .get_matches();

    if let Some(i) = matches.value_of("INPUT") {
        let content = fs::read_to_string(i).expect("Something went wrong reading the file");
        let tokenizer = Tokenizer::new(&content);

        // TODO - probably have better logging here
        // fs::write("test.tokens", format!("{:#?}", tokens)).unwrap();

        // parse tokens and generate AST
        let ast = Parser::new(tokenizer).parse().unwrap();
        fs::write("test.ast", format!("{:#?}", ast)).unwrap();

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
}

// fn native_println() {
//     println!("Hello world");
// }
