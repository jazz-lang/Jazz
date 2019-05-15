# Variables

## Declaration

A simple variable declaration looks like this:
```go
var foo = 42;
```
We can also specify the type of the variable explicitly:
```go
var foo: i32 = 42;
```

## Zero-initialized declaration

To declare variable with zero-initialized value you can do this:
```go
var foo: i32;
```
Now `foo` value is `0`. If you will use as type some struct type you will get segfault if try to use values from this struct.
If you will set type to pointer `foo` value will be `null`.

## Assignment
To reassign a new value to variable:
```go

foo = 42 + 2;
```


## Global variables
Global variables defined in global context using `var` keyword:
```go
var counter = 0;

pub func main() void {
    counter = counter + 1;

    return;
}
```
You can mark global variable as external and linker try to resolve symbol:
```go
import "std/libc.jazz" 

extern var stdout: *FILE; // stdout,stdin and stderr already defined in libc.jazz

pub func main() void {
    fwrite(stdout,"Hello,world!\n");
    return;
}
```