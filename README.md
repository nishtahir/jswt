# JSWT

[![Rust](https://github.com/nishtahir/jswt/actions/workflows/rust.yml/badge.svg)](https://github.com/nishtahir/jswt/actions/workflows/rust.yml)

An experiment in compilation targetting WebAssembly.

## Demo

```
// foo.jswt

let globalVar = 0;

// Inline Wast support with named params
@wast("(global.set $globalVar (local.get $a))")
function setGlobalVar(a: i32) {}
```

```
import "./foo.jswt"

// main.jswt
export function main(): i32 {
    let a = 123;
    if (a == 123) {
        setGlobalVar(123);
    } else {
        println(99);
    }

    println(globalVar)
}
```

```
jswt ./main.jswt
```

## Usage

```
USAGE:
    jswt [FLAGS] <source>

FLAGS:
    -h, --help       Prints help information
        --log-mem    Log the memory layout after execution
    -V, --version    Prints version information

ARGS:
    <source>    Input file to begin compiling
```
## Resources

* [Official Web Assembly Specification](https://webassembly.github.io/spec/core/)
* [Web Assembly Reference Manual](https://github.com/sunfishcode/wasm-reference-manual)
* [Writing WebAssembly By Hand](https://blog.scottlogic.com/2018/04/26/webassembly-by-hand.html)