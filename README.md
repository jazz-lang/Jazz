# Jazz

Jazz - statically typed programming language

# Goals

- JIT and AOT compilation
- Optional GC and automatic memory managment 
- Simple and clear syntax
- Bootsraping compiler

# Syntax

```swift
func change(value: ref int,to: int) {
  value = to;
}

func add(x: int,y: int): int {
  return x + y;
}

func main(): int {
    var x: int = 2 * 2;
    var y = x * x;
    change(x,add(x,y));
} 

```

```swift

struct Point  {
    x: int;
    y: int;
}
class PointClass {
     public var point: Point;
     
     this(x: int,y: int) {
        point.x = x;
        point y = y;
     }
}

```
```swift
func main(): int {
    var ptr: int* = malloc(4);
    var integer: int = ptr as int;
}
```
