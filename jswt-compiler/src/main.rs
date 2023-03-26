mod env;

use clap::Parser;
use jswt_ast::Ast;
use jswt_ast_serializer::AstSerializer;
use jswt_codegen::CodeGenerator;
use jswt_errors::{print_parser_error, print_semantic_error, print_tokenizer_error};
use jswt_hir_lowering::HirLoweringContext;
use jswt_mir_lowering::MirLoweringContext;
use jswt_parser::Parser as JswtParser;
use jswt_semantics::GlobalSemanticResolver;
use jswt_semantics::LocalSemanticResolver;
use jswt_semantics::TypeInferenceResolver;
use jswt_symbols::BindingsTable;
use jswt_symbols::ScopedSymbolTable;
use jswt_tokenizer::Tokenizer;
use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::process::exit;
use wasmer::{imports, Function, Instance, MemoryView, Module as WasmerModule, Store};

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
    #[arg(short, long, help = "Write output to file")]
    output: Option<PathBuf>,

    #[arg(help = "Write output to file")]
    file: PathBuf,

    #[arg(
        short,
        long,
        help = "Write the generated WAST",
        default_value = "false"
    )]
    wast: bool,

    #[arg(
        short,
        long,
        help = "Log the memory layout after execution",
        default_value = "false"
    )]
    log_mem: bool,

    #[arg(short, long, help = "Minify WAST output", default_value = "false")]
    minified: bool,

    #[arg(
        short,
        long,
        help = "Do not include the runtime and stdlib",
        default_value = "false"
    )]
    no_std: bool,

    #[arg(short, long, help = "Path to runtime sources")]
    runtime_path: Option<PathBuf>,
}

fn main() {
    let Args {
        output,
        file,
        wast,
        log_mem,
        minified,
        no_std,
        runtime_path,
    } = Args::parse();

    let output = match output {
        Some(it) => PathBuf::from(it),
        None => file.clone(),
    };

    let runtime = if no_std {
        // Don't include the runtime even if the path is provided
        None
    } else {
        // If a runtime path isn't provided use the default path
        let default = PathBuf::from("./runtime/rt.jswt");
        runtime_path
            .or(Some(default))
            .map(fs::canonicalize)
            .map(Result::unwrap)
    };

    let ast = compile_module(&file, &output, runtime.as_ref());
    let mut code_gen = CodeGenerator::default();
    let module = code_gen.generate_module(&ast);

    // Write generated wasm AST for debugging
    let stringified_wast = module.as_wat(minified);

    if wast {
        fs::write(output.with_extension("wast"), &stringified_wast).unwrap();
    }

    // Embed wasmer runtime and execute generated wasm
    let mut store = Store::default();
    let module = match WasmerModule::new(&store, stringified_wast) {
        Ok(module) => module,
        Err(e) => {
            panic!("{}", e);
        }
    };

    let imports = imports! {
        "env" => {
            "println" => Function::new_typed(&mut store, env::println),
            "exit" => Function::new_typed(&mut store, env::exit),
            "assertEqual" => Function::new_typed(&mut store, env::assert_equal)
        },
    };
    let instance = Instance::new(&mut store, &module, &imports).unwrap();

    // Assume main is a function that accepts no args and returns an i32
    // The returned i32 is the exit code
    // function main(): i32 { return 0; } // OK
    match instance.exports.get_function("main") {
        Err(wasmer::ExportError::Missing(_)) => {
            println!("Missing main function. Did you forget to export it?");
            exit(1);
        }
        Err(e) => {
            panic!("{}", e);
        }
        Ok(main) => {
            let result = match main.call(&mut store, &mut []) {
                Ok(result) => result,
                Err(e) => {
                    panic!("{}", e);
                }
            };

            if log_mem {
                // Log memory contents to a file
                let memory = instance.exports.get_memory("memory").unwrap();

                let view: MemoryView = memory.view(&store);
                let mut slice = vec![];

                for i in 0..view.data_size() {
                    slice.push(view.read_u8(i).unwrap());
                }

                let mem = slice
                    // log the memory with 16 bytes per row
                    // This will give us 4096 rows per page
                    .chunks(16)
                    .map(|chunk| {
                        chunk
                            .iter()
                            .map(|val| format!("{:02x?}", val))
                            .collect::<Vec<String>>()
                            .join(" ")
                    })
                    .enumerate()
                    .map(|(i, line)| format!("{:04X?}0  {}", i, line))
                    .collect::<Vec<String>>()
                    .join("\n");

                fs::write(output.with_extension("mem"), &mem).unwrap();
            }

            if result.len() > 0 {
                let exit_code = result[0].i32().unwrap_or(1);
                exit(exit_code);
            }
            // No exit code will panic during the validation but for safety
            // if no exit code was given assume there was a problem
            exit(1);
        }
    }
}

fn compile_module(input: &Path, output: &Path, runtime: Option<&PathBuf>) -> Ast {
    // Let binding to prevent the ref being dropped before getting passed to the tokenizer
    let mut tokenizer = Tokenizer::new();
    // Sources root for user defined sources is the current directory
    // from where the compiler is being invoked.
    tokenizer.set_sources_root(Some(&std::env::current_dir().unwrap()));
    tokenizer.set_module_prefix(Some("module".to_string()));
    tokenizer.enqueue_source_file(input);

    if let Some(runtime) = runtime {
        // Sources at the top of the source stack will be resolved first
        // This has implications in the parsing and semantic analysis process
        // As errors related to redefinitions and shadowing
        // should point to user defined sources
        tokenizer.set_sources_root(runtime.parent().map(Path::to_path_buf).as_ref());
        tokenizer.set_module_prefix(Some("runtime".to_string()));
        tokenizer.enqueue_source_file(runtime);
    }

    let mut parser = JswtParser::new(&mut tokenizer);
    let ast = parser.parse();

    // Write AST for debugging
    fs::write(output.with_extension("ast"), format!("{:#?}", ast)).unwrap();

    let mut has_errors = false;

    // Report errors
    for error in parser.tokenizer_errors() {
        has_errors = true;
        print_tokenizer_error(&error);
    }

    for error in parser.parse_errors() {
        has_errors = true;
        print_parser_error(&error);
    }

    let mut symbol_table = ScopedSymbolTable::default();
    let mut bindings_table = BindingsTable::default();

    // Global Semantic analytis pass
    // This pass resolves classes, functions, variables and imports
    let mut global = GlobalSemanticResolver::new(&mut bindings_table, &mut symbol_table);
    global.resolve(&ast);

    for error in global.errors() {
        has_errors = true;
        print_semantic_error(&error);
    }

    if has_errors {
        exit(1);
    }

    // Hir lowering pass to desugar operators.
    let mut lowering = HirLoweringContext::new(&mut bindings_table, &mut symbol_table);
    let mut ast = lowering.lower(&ast);
    fs::write(output.with_extension("hir.ast"), format!("{:#?}", ast)).unwrap();

    // Perform type checking and inference
    // annotate the AST with resolved and inferred types
    let mut types = TypeInferenceResolver::new(&mut bindings_table, &mut symbol_table);
    types.resolve(&mut ast);

    // Local semantic analysis pass to resolve local variables
    // and perform deeper type checking
    let mut local = LocalSemanticResolver::new(&mut bindings_table, &mut symbol_table);
    local.resolve(&ast);

    for error in local.errors() {
        has_errors = true;
        print_semantic_error(&error);
    }

    if has_errors {
        exit(1);
    }

    let mut serializer = AstSerializer::default();
    let content = serializer.serialze(&ast);
    fs::write(output.with_extension("hir.jswt"), content).unwrap();

    // Mir lowering pass to generate the lower intermediate representation
    let mut mir_lowering = MirLoweringContext::new(&mut bindings_table, &mut symbol_table);
    let ast = mir_lowering.lower(&ast);

    if has_errors {
        exit(1);
    }

    let mut serializer = AstSerializer::default();
    let content = serializer.serialze(&ast);
    fs::write(output.with_extension("mir.jswt"), content).unwrap();

    ast
}

#[cfg(test)]
mod test {
    use assert_cmd::prelude::{CommandCargoExt, OutputAssertExt};
    use jswt_assert::{assert_snapshot, assert_str_eq};
    use regex::Regex;
    use std::{borrow::Cow, process::Command};

    #[test]
    #[ignore]
    fn test_compile_and_execute_variables_sample() {
        let mut cmd = Command::cargo_bin("jswt").unwrap();
        cmd.arg("--runtime-path")
            .arg("../runtime/rt.jswt")
            .arg("../example/variables.jswt")
            .assert()
            .success();
    }

    #[test]
    #[ignore]
    fn test_compile_and_execute_loops_sample() {
        let mut cmd = Command::cargo_bin("jswt").unwrap();
        let assert = cmd
            .arg("--runtime-path")
            .arg("../runtime/rt.jswt")
            .arg("../example/loops.jswt")
            .assert()
            .success();
        let stdout = std::str::from_utf8(&assert.get_output().stdout).unwrap();
        assert_str_eq!(stdout, "45\n");
    }

    #[test]
    #[ignore]
    fn test_compile_and_execute_arithmetics_sample() {
        let mut cmd = Command::cargo_bin("jswt").unwrap();
        let assert = cmd
            .arg("--runtime-path")
            .arg("../runtime/rt.jswt")
            .arg("../example/math.jswt")
            .assert()
            .success();
        let stdout = std::str::from_utf8(&assert.get_output().stdout).unwrap();
        assert_str_eq!(stdout, "7\n1\n12\n1\n");
    }

    #[test]
    #[ignore]
    fn test_compile_and_execute_arrays_sample() {
        let mut cmd = Command::cargo_bin("jswt").unwrap();
        let assert = cmd
            .arg("--runtime-path")
            .arg("../runtime/rt.jswt")
            .arg("../example/arrays.jswt")
            .assert()
            .success();
        let stdout = std::str::from_utf8(&assert.get_output().stdout).unwrap();
        assert_str_eq!(stdout, "99\n2\n3\n4\n");
    }

    #[test]
    #[ignore]
    fn test_variable_not_found_semantic_error() {
        let mut cmd = Command::cargo_bin("jswt").unwrap();
        let assert = cmd
            .arg("--runtime-path")
            .arg("../runtime/rt.jswt")
            .arg("./test/variable-not-found.jswt")
            .assert()
            .failure();
        let stdout = std::str::from_utf8(&assert.get_output().stdout).unwrap();
        assert_snapshot!(redact_paths(stdout));
    }

    /// Simple redaction of paths that will be generated by errors
    /// in the crate to keeps tests that may run on different systems
    /// from failing.
    fn redact_paths(value: &str) -> Cow<str> {
        let re = Regex::new(r"/[\w/]+/jswt/jswt-compiler/").unwrap();
        re.replace_all(value, "[redacted]/jswt/jswt-compiler/")
    }
}
