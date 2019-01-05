# Jazz

Jazz - simple programming language

# Goals

- JIT and AOT compilation
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
