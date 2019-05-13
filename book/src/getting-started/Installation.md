# Installation

## Build from source

First step is clone source code from github repository and build project using cargo: 

```
$ git clone https://github.com/jazz-lang/jazz
$ cd jazz
$ cargo build --release
$ cargo install --path . --force
```

Next step is build Jazz C-based std that provides platform-dependent constants (RAND_MAX and others):
```bash
# use your favorite compiler and emit shared library
$ cc -shared std/jazz_cstd.c -o libjazzstd.so
```
After this we should copy shared library into /usr/lib folder to allow linker link this library:
```
$ sudo cp libjazzstd.so /usr/lib
$ sudo cp libjazzstd.so /usr/lib64
```