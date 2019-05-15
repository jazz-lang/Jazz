# Using compiler

Jazz compiler got these options:
```
Jazz language compiler

USAGE:
    jazz [FLAGS] [OPTIONS] <file>

FLAGS:
        --emit-asm       Print assembly to stdout
        --emit-gimple    Dump GIMPLE to stdout if gccjit backend used
        --emit-obj       Output object file
    -h, --help           Prints help information
        --jit            Use JIT compilation instead of AOT compilation
        --shared         Output shared library (.dll or .so)
    -V, --version        Prints version information

OPTIONS:
        --backend <backend>           Select backend [default: gccjit]  [possible values: gccjit, cranelift, cpp]
    -f <gcc_opts>...                  
    -l, --link <libraries_link>...    
    -O, --opt-level <opt_level>       Set optimization level [default: 2]
    -o, --output <output>             Set output filename

ARGS:
    <file>    
```

## Compiling file
To compile file you can just do this:
```
$ jazz filename.jazz
```
Or you can select backend or use jit and add additional options
```
$ jazz -O3 --jit --emit-gimple filename.jazz
```

