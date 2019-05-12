# Jazz

Jazz - JIT/AOT compiled programming language that uses gccjit for emitting machine code.


# TODO
- Jazz2C++ Translator
- ~~Struct initialization:~~ DONE!

# Example code

Factorial: 
```go
import "std/libc.jazz"
func factorial(x: i32) i32 {
    if x == 0 {
        return 1;
    } else {
        return factorial(x - 1) * x;
    }
}

pub func main() i32 {
    printf("%i\n",factorial(5));

    return 0;
} 
```