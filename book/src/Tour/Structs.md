# Structs

New struct types can be defined with the `struct` keyword:
```go
struct Foo {
    foo: i32
}
```

## Member functions
Member functions are just like non-member functions, except that they receive a this parameter (aka the receiver) which is passed using the dot notation, when calling the function.

```go
func (v: *Point) foo() void {
    // ...
}

var v = Point {};

v.foo();
```

