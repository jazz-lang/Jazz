# Hello,world!

Let's write our first program!

```go
import "std/display.jazz"

pub func main() i32 {
    println("Hello,world!");
    return 0;   
}

```

Save the file and go back to your terminal window. Enter the following commands to run file:
```
$ jazz --jit file.jazz
```
Or if you want to emit executable binary:
```
$ jazz file.jazz
```


## Anatomy of Jazz program

First thing that we written in `import "std/display.jazz`, import statement "includes" public statements into current file allowing access contents of 
imported file.

The next thing is `pub func main() i32`. We declared public function that will be seen outside of module on linkage with return type `i32` and name `main`.

(`main` function should always declared as public function)

Inside the `main` function is the following code:
```go
println("Hello,world!");
return 0;
```

First line does all the work in program: it prints text to the screen (stdout).
Second line just return zero from main function, if you run this code with `--jit` option you will see this text:
```
Hello,world!
Exit value: 0
```

