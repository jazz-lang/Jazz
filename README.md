# Jazz

Jazz is a statically typed programming language that allows you emitting machine code or translating Jazz AST directly to C++.

# Learning
 
To learn language you need clone Jazz source code from github and build book using `mdbook`,you can look at how to build book in `README.md` in `master`.

## GCCJIT
GCCJIT is a wrapper around GCC that allows emitting GIMPLE code. Jazz AST directly translated into GIMPLE and then compiled into machine code,after this binary file or JIT code emitted.

## C++
Jazz also translates its AST into C++ and then this output compiled by default C++ compiler.

## Cranelift and LLVM
These two backends still unimplemented since for this backend Jazz should get a nice IR to perform optimizations on code.

## Example code
Jazz looks like this:
```go
import "std/libc.jazz"

struct Point {
    x: i32,
    y: i32
}

func add(x: i32,y: i32) i32 {

    return x + y;
}

func add(x: f32,y: f32) f32 {
    return x + y;
}

pub func main() i32 {
    var p = Point {
        x: 40,
        y: 2
    }
    printf("%i\n",add(p.x,p.y));   
    printf("%f\n",add(4.0,2.0));
    return 0;
}
```