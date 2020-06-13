# Constexpr


## Constexpr global

`comptime` allows you declare a global variable without type that just "inserted" into code when compiling:
```go
comptime VAL = 2 * 2

func foo() i32 {
    return VAL;
}
```
