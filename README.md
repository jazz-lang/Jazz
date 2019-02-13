# Jazz

Jazz is a dynamically typed programming language inspired by Lua, Neko and Rust

# Learning

## Values
- Nil: the special value nil is used for uninitialized variables
- Integer: integer can be represented in decimal form,or hexadecimal
- Floats: floating point number, can be represented using a period
- Boolean: represented by two keywords: true or false
- String: strings are surrounded by double quotes, example: "Hello,world!"
- Arrays: dynamically sized array indexed by integers
- Objects: an object is a table,which associates `Value : Value`
- Functions: a function is also value and can be stored in variables

## Comments
You can write comments like this:
```
// your comment there
```
Or:
```
/* your comment there */
```
P.S We using OCaml syntax highlighting so in example code used Ocaml comments

## Variables
### Local scope
The local scope contains all variables defined with the "var" or "let" keyword in previous syntactical blocks.
Example:
```ocaml
var x = 2 + 2
println(x)
```
Function parameters are also local variables. They are defined within the whole function:
```ocaml
var x = 3
let f = |x| println(x)
f("jazz") (* print "jazz" *)
```

### Global scope
When a variable is not found in the local scope or in the local function environment, it's a global variable.

## Return
Jazz supports implicit returns, this means you can do this:
```ocaml
let mul = |x,y| x * y
```
But you can still use `return` keyword if you want:
```ocaml
let mul = |x,y| return x * y
```

## Function

You can define new function is very easy, you cana assign this function to a new variable:
```ocaml
let add = |x,y| x + y
```
Or you can do this:
```ocaml
let value = (|x,y| x + y)(2,3) (* calls new function and return 2 + 3 *)
```

## Objects
Objects are a hashtable that looks like this: `Table<Value(Key),Value>`. To create object you can use builtin `new` function which initializes new object
```ocaml
var my_obj = new(nil) (* new empty object *)
var my_obj = new(my_proto) (*new object from prototype *) 
```




# Builtins
- `string(any) -> str`: Converts value to string
- `print(...) -> int`: Print arguments to stdout and return count of arguments
- `println(...) -> int`: Print arguments to stdout and return count of arguments
- `anew(int?) -> array`: Initialize new empty array or new array with some size
- `apop(array) -> any`: Pop last element from top of array and return it
- `apush(array,any)`: Push value at top of the array
- `aget(array,int) -> any`: Returns value at index 
- `aset(array,int,any)`: Set valuet at index
- `alen(array) -> int`: Return array length
- `objset(object,any,any)`: TODO
- `objget(object,any) -> any`: TODO



# Virtual Machine
Jazz uses Waffle VM. Waffle is a dynamically typed stack-based VM that includes simple mark-and-sweep garbage collector.

## Opcodes
TODO