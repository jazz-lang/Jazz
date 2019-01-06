# PEACE

PEACE (Peak Compiler) - library for emitting x86_64 code using jazz-jit library


# Features
- You can import functions or other data from dynamic library
```rust
    module.declare_function("main".into(), Linkage::Local);
    module.declare_function("puts".into(), Linkage::Dylib("/usr/lib64/libc++.so.1".into())); // Yes, loading a c++ functions supported too
    let func = module.get_function(&"main".to_string());

    let string = func.iconst(Int(64),b"Hello,world!".as_ptr() as i64);
    let v1 = func.call_indirect("puts",&[string], Int(32));
    func.ret(v1);
```
- Register allocation on the fly

```rust
let int = Type::Int(32);

let v1 = func.iconst(int,4);
let v2 = func.iconst(int,2);
let v3 = func.imul(v1,v2);
let v4 = func.iconst(int,4);
let v5 = func.iadd(v4,v3);
func.ret(v5);

```
Assembly:
```assembly
0x0: pushq %rbp
0x1: movq %rsp, %rbp
0x4: movl $4, %ecx
0x9: movl $2, %r8d
0xf: imull %r8d, %ecx
0x13: movl $4, %r8d
0x19: addl %ecx, %r8d
0x1c: movl %r8d, %ecx
0x1f: movl %ecx, %eax
0x21: popq %rbp
0x22: retq
```

# Limitations

- There are no liveness analysis,after using `Value` just "destroyed"
- Only integers for now supported


