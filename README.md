## Usane - unsigned integers for OCaml

%%VERSION%%
The behaviour of numbers (int, int32, int64) in OCaml is to be signed, and wrap
around on over/underflow.  This library gives explicit access to the carry bit
by using compiler builtins.  Other fine integer libraries, such as
[integers](https://github.com/ocamllabs/ocaml-integers) library and
[stdint](https://github.com/andrenth/ocaml-stdint)) mirror the OCaml standard
library behaviour.

This library defines 8, 16, 32, and 64 bit unsigned integers which interoperates
well with the builtin types (int32, int64) by reusing their representation.
Arithmetic operations return the carry bit explicitly.

Some motivating examples are:
```OCaml
# succ max_int
- : int = -4611686018427387904

# pred min_int
- : int = 4611686018427387903

# abs min_int
- : int = -4611686018427387904
```

Usane comes with a extensive test suite to show the behaviour.

Currently this library is not released, I first want to a) use
[ocb-stubblr](https://github.com/pqwy/ocb-stubblr.git) (to get it to run on
MirageOS) and b) test on 32bit.

## Documentation

[![Build Status](https://travis-ci.org/hannesm/usane.svg?branch=master)](https://travis-ci.org/hannesm/usane)

[API Documentation](https://hannesm.github.io/usane/doc/) is available online.
