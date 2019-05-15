# Types

Every value in Jazz is of some certain type. We'll look at two data types: scalar and compound  

# Scalar Types
A scalar type represents a single value. Jazz got four primary scalar types: floating-point numbers,booleans,integer nubmers and character types.

## Integer types

Jazz got these integer types:
- i8: signed byte type with range -255..255
- i16: signed short type with range -32768..32767
- i32: signed int type with range -2147483648..2147483647
- i64: signed long type with range -9223372036854775808..9223372036854775807
- u8: unsigned byte type with range 0..255
- u16: unsigned short type with range 0..32767
- u32: unsigned int type with range 0..2147483647
- u64: unsigned long type with range: 9223372036854775807
- usize: unsigned type that got size depending on current architecture

## Floating-point types

- f32: 32-bits size floating point
- f64: 64-bits size floating point
- f128: 128 bits size floating point (UNIMPLEMENTED!)

## Boolean type
A boolean type in Jazz has two possible values: true and false. Booleans are one byte in size. The Boolean type in Rust is specified using bool. 
```go
pub func main() void {
    var v0 = true;
    var v1: bool = !v0;
    return;
}

```

## Character type

TBD

# Compound types

## The array type
(This type not yet implemented for now and you can't use it)


To have a collection of multiple values you can use array. Every element of an array must have the same type.
```go
pub func main() void {
    var array: [5]i32 = [1,2,3,4,5];

    return;
}
```

