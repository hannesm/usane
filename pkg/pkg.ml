#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "usane" @@ fun c ->
  Ok [
    Pkg.mllib "src/usane.mllib";
    Pkg.clib "src/libusane_stubs.clib";
    Pkg.test "test/test"
  ]
