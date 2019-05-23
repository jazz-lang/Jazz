# Unions

A union declaration uses the same syntax as a struct declaration, except with `union` in place of `struct`.

```rust
union Value {
    i: i32,
    f: f32
}
```

The key property of unions is that all fields of a union share common storage. As a result writes to one field of a union can overwrite its other fields, and size of a union is determined by the size of its largest field.

To create union you should declare variable and then set one of field:
```ts
var val: Value;
val.i = 42;
```

