<div>
<h1 align="center">JSWT</h1>
<p align="center">
    <a href="https://github.com/nishtahir/jswt/actions/workflows/rust.yml" alt="Build">
        <img src="https://github.com/nishtahir/jswt/actions/workflows/rust.yml/badge.svg" /></a>
    <a href="https://codecov.io/gh/nishtahir/jswt" alt="Coverage">
        <img src="https://codecov.io/gh/nishtahir/jswt/branch/master/graph/badge.svg?token=VL6CT8HU4U" /></a>
</p>
</div>

A JavaScript/TypeScript like language that compiles to web assembly. 

## Architecture

Compilation takes place in phases described in the following chart. Each of these phases roughly takes place in a module of a similar name.

[![](https://mermaid.ink/img/pako:eNptkU1ugzAQha9ieeVKSQ_AohIBSpBYVAGpi5CFgUlwYmzkH0U0yt1rIG1ExKw88z7PvLFvuJI1YA-fFO0alO4KgVz4-0R01ryf9dUc0Hr9gTaTMIS25UR_UaWZOD2VITYklxcQ7AfU23gzIAPnsicHop6SYCTCvZ_lh6kSjpVoYVoGLRWGVcgXlPea6fnciMRclpSjrG9LydEOtOTWMCkmG58kldWiPO8zwjHJ-w5Q0EB1cRsuWY9HcDtYR6m8gnLcY4ftKCUkcE-LYhCg6Mug_6VekLmVhHwP3Rc7OB94hVtQLWW1-7_bIBXYNNBCgT13rOFILTcFLsTdobarqYGoZkYq7B0p17DC1BqZ9aLCnlEW_qCQUeeufVD3X6HUoa4)](https://mermaid.live/edit#pako:eNptkU1ugzAQha9ieeVKSQ_AohIBSpBYVAGpi5CFgUlwYmzkH0U0yt1rIG1ExKw88z7PvLFvuJI1YA-fFO0alO4KgVz4-0R01ryf9dUc0Hr9gTaTMIS25UR_UaWZOD2VITYklxcQ7AfU23gzIAPnsicHop6SYCTCvZ_lh6kSjpVoYVoGLRWGVcgXlPea6fnciMRclpSjrG9LydEOtOTWMCkmG58kldWiPO8zwjHJ-w5Q0EB1cRsuWY9HcDtYR6m8gnLcY4ftKCUkcE-LYhCg6Mug_6VekLmVhHwP3Rc7OB94hVtQLWW1-7_bIBXYNNBCgT13rOFILTcFLsTdobarqYGoZkYq7B0p17DC1BqZ9aLCnlEW_qCQUeeufVD3X6HUoa4)

[Wasmer](https://wasmer.io/) is used as the WASM execution engine.

## CLI

```
USAGE:
    jswt [FLAGS] [OPTIONS] <file>

FLAGS:
    -h, --help        Prints help information
        --log-mem     Log the memory layout after execution
        --minified    Minify WAST output
        --no-std      Do not include the runtime and stdlib
    -V, --version     Prints version information

OPTIONS:
    -o <output>                          Write output to file
        --runtime-path <runtime-path>    Path to runtime sources

ARGS:
    <file>    Input file to begin compiling
```

# Sample

```
class Array {
    length: i32;
    capacity: i32;
    data: i32;

    constructor(capacity: i32) {
        this.length = 0;
        this.capacity = capacity;
        this.data = malloc(capacity * 4);
    }

    set(index: i32, data: i32) {
        i32Store(this.data + index * 4, data);
    }

    get(index: i32): i32 {
        return i32Load(this.data + index * 4);
    }
}

export function main(): i32 {
    let value: i32 = 10;
    let value2 = value + 33;

    println(value2);

    return 0;
}
```

## Resources

* [Official Web Assembly Specification](https://webassembly.github.io/spec/core/)
* [Web Assembly Reference Manual](https://github.com/sunfishcode/wasm-reference-manual)
* [Writing WebAssembly By Hand](https://blog.scottlogic.com/2018/04/26/webassembly-by-hand.html)
* [Wasmer](https://wasmer.io/)