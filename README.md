# Toml (OCaml parser for TOML)
[![Build Status](https://travis-ci.org/sagotch/To.ml.png?branch=master)](https://travis-ci.org/sagotch/To.ml)
[![Coverage Status](https://coveralls.io/repos/sagotch/To.ml/badge.png)](https://coveralls.io/r/sagotch/To.ml)

Ocaml parser for TOML [(Tom's Obvious Minimal Language)](https://github.com/mojombo/toml) v0.2.0.

This library can also serialize a TOML structure to string.

## Dependencies

Toml need Ocaml 4.0 at least. Check your local installation with `ocamlc -v`.

This project use **ocamllex** and **menhir** parsing features. In order to
compile tests you will also need **OUnit** and **bisect** is required to 
generate code coverage summary.

## Install

* Via Opam: `opam install toml`
* From source:
```
git clone https://github.com/sagotch/To.ml
cd To.ml
make build
make install
```
make install may need sudo.

## Usage

`open Toml` in your file(s), and link toml library when compiling. For 
instance, using ocamlbuild:
```
ocamlbuild -use-ocamlfind -package toml foo.byte
```

## Limitations

* Array of tables is not supported.
