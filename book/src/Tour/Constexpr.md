# Constexpr


## Constexpr global

`constexpr` allows you declare a global variable without type that just "inserted" into code when compiling:
```go
constexpr VAL = 2 * 2

func foo() i32 {
    return VAL;
}
```
