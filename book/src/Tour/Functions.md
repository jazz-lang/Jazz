# Functions

Functions are declared using the `func` keyword:
```go
func foo() void {}
```
Parameters go inside parentheses,example:
```go
func foo(a: i32,b: i32) void {}
```
Also function always need specify return type, if you don't want to return anything from your function you should use `void` type as return type.
```go
func add(x: i32,y: i32) i32 {
    return x + y;
}
```

You always need place return in "unterminated" blocks in functions.

# Methods

To declare method function you need use this syntax:
```go
func (v: *StructType) foo() void {}
```
Currently methods works only with C++ backend.

# Function type
To declare function type you can use following syntax:
```
(param_type) -> return_type
```

# Referencing functions
To get reference to function you need use `func &` expression:
```go
var foo: (i32,i32) -> i32 = func &foo
```
Be careful since Jazz don't match function args in this expression.


## Function modifiers

- `pub`:
    Declares function as public,this allows using this function from some static or shared library if you link it with your file.
- `inline`:
    Makes function always inline so you can't use this function from other files.
- `extern`:
    Marks function as external, this allows using functions from C or other languages:
    ```go
        extern func printf(fmt: *char,...) void;
    ```
