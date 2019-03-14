# Jazz
Language designed for embeddin, making games and desktop programs including optional GC and JIT compiler

# UnsafeJazz
Version of Jazz that allows writing "low level" programs, e.g you allowed to allocate memroy manually


# Examples

```javascript
open "std/io"

function main() {
  writeln("Hello,world!");
}
```

```javascript
function add(x: ref int,y: ref int,z: ref int) {
   *x = *y = *z;
}

function main(): int {
  var x,y,z  = 0;
  
  add(&x,&y,&z);
   return x;
}
```

```javascript
#unsafe // this thing needed for calling functions such as `alloc`,`transmute` and etc


struct Point {
  x: int,
  y: int
}

function new_point(x,y: int): ref Point {
  var ptr = alloc<Point>(); // allocates memory, if GC enabled alloacte memory using GC otherwise use malloc
  ptr.x = x;
  ptr.y = y;
  return ptr;
}
```

