# Control flow

## if
An if-statement looks like this:
```go
if cond_a {
    // ...
} else if cond_b {
    // ...
} else {
    // ...
}
```

## for(UNIMPLEMENTED)
There is C-style for-loop with this syntax:
```go
for var i = 0; i < 42; i = i + 1 {
    // ...
}
```
## foreach (UNIMPLEMENTED)

There is Rust like `for` loop that allows iterating values in types which got `iter` method.
```go

foreach val : values {
    // ...
}

```

## While

A while loop has the following syntax:
```c
while cond {
    // ...
}
```
If you need infinity loop you don't need use `while true` instead of this use `loop` statement. (Compiler give you warning that you can use `loop`)

## Loop
`loop` creates infinity loop.
```rust
loop {
    // ...
    if cond {break}
}

```

## break
Inside loop you can transfer control flow outisde the loop with `break` statement.

## continue
Inside loops you can skip the rest of the loop body and start executing the next iteration with the `continue` statement.