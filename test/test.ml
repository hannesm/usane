open Usane

let uint32 =
  let module M = struct
    type t = Uint32.t
    let pp = Uint32.pp
    let equal s t = Uint32.compare s t = 0
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let is_zero () = Alcotest.check uint32 "zarro" 0l Uint32.zero
let is_one () = Alcotest.check uint32 "uno" 1l Uint32.one

let of_int_r () =
  for i = 0 to 1000 do
    let r = Nocrypto.Rng.Int.gen Int32.(to_int max_int) in
    Alcotest.check uint32 "random" (Int32.of_int r) (Uint32.of_int r)
  done

let int_bound () =
  Alcotest.check uint32 "int 0 ok" 0l (Uint32.of_int 0) ;
  Alcotest.check_raises "smaller 0 raises"
    (Invalid_argument "out of range")
    (fun () -> ignore (Uint32.of_int (-1))) ;
  Alcotest.check uint32 "int 2 ^ 32 - 1 ok" 0xFFFFFFFFl (Uint32.of_int 0xFFFFFFFF) ;
  Alcotest.check_raises "greater 2 ^ 32 - 1 raises"
    (Invalid_argument "out of range")
    (fun () -> ignore (Uint32.of_int (0x100000000)))

let to_of_int () =
  for i = 0 to 1000 do
    let r = Nocrypto.Rng.Int.gen 0xFFFFFFFF in
    Alcotest.(check int "to_int (of_int x) works" r Uint32.(to_int (of_int r)))
  done

let add_ints () =
  for i = 0 to 1000 do
    let a = Nocrypto.Rng.Int.gen 0xFFFFFFFF in
    let b = Nocrypto.Rng.Int.gen (0xFFFFFFFF - a) in
    Alcotest.check uint32 "add works" (Uint32.of_int (a + b))
      Uint32.(add (of_int a) (of_int b))
  done

let add_int_overflow () =
  Alcotest.check_raises "add 0xFFFFFFFF 1 raises"
    (Invalid_argument "overflow")
    (fun () -> ignore Uint32.(add (of_int 0xFFFFFFFF) one)) ;
  Alcotest.check_raises "succ 0xFFFFFFFF raises"
    (Invalid_argument "overflow")
    (fun () -> ignore Uint32.(succ (of_int 0xFFFFFFFF))) ;
  Alcotest.check_raises "add 0x800000000 0x80000000 raises"
    (Invalid_argument "overflow")
    (fun () -> ignore Uint32.(add (of_int 0x80000000) (of_int 0x80000000))) ;
  Alcotest.check uint32 "add 0x800000000 0x7FFFFFFF is good"
    0xFFFFFFFFl
    Uint32.(add (of_int 0x80000000) (of_int 0x7FFFFFFF))

let add_int_wrap () =
  Alcotest.check uint32 "add_wrap 0xFFFFFFFF 1 is 0"
    0l
    Uint32.(add_wrap (of_int 0xFFFFFFFF) one) ;
  Alcotest.check uint32 "add_wrap 0x800000000 0x80000000 is 0"
    0l
    Uint32.(add_wrap (of_int 0x80000000) (of_int 0x80000000)) ;
  Alcotest.check uint32 "add_wrap 0x800000000 0x7FFFFFFF is good"
    0xFFFFFFFFl
    Uint32.(add_wrap (of_int 0x80000000) (of_int 0x7FFFFFFF))

let sub_int () =
  for i = 0 to 1000 do
    let a = Nocrypto.Rng.Int.gen 0xFFFFFFFF in
    let b = Nocrypto.Rng.Int.gen a in
    Alcotest.check uint32 "sub works" (Uint32.of_int (a - b))
      Uint32.(sub (of_int a) (of_int b))
  done

let sub_int_underflow () =
  Alcotest.check_raises "sub 0 1 raises"
    (Invalid_argument "underflow")
    (fun () -> ignore Uint32.(sub zero one)) ;
  Alcotest.check_raises "pred 0 raises"
    (Invalid_argument "underflow")
    (fun () -> ignore Uint32.(pred zero)) ;
  Alcotest.check_raises "sub 0x800000000 0x80000001 raises"
    (Invalid_argument "underflow")
    (fun () -> ignore Uint32.(sub (of_int 0x80000000) (of_int 0x80000001))) ;
  Alcotest.check uint32 "sub 0x800000000 0x7FFFFFFF is 1"
    1l
    Uint32.(sub (of_int 0x80000000) (of_int 0x7FFFFFFF))

let sub_int_wrap () =
  Alcotest.check uint32 "sub_wrap 0 1 is 0xFFFFFFFF"
    0xFFFFFFFFl
    Uint32.(sub_wrap zero one) ;
  Alcotest.check uint32 "sub 0x800000000 0x80000001 is 0xFFFFFFFF"
    0xFFFFFFFFl
    Uint32.(sub_wrap (of_int 0x80000000) (of_int 0x80000001)) ;
  Alcotest.check uint32 "sub 0x800000000 0x7FFFFFFF is 1"
    1l
    Uint32.(sub_wrap (of_int 0x80000000) (of_int 0x7FFFFFFF))

let compare_works () =
  Alcotest.check Alcotest.int "compare 0xFFFFFFFF 0xFFFFFFFF is 0"
    0 Uint32.(compare (of_int 0xFFFFFFFF) (of_int 0xFFFFFFFF)) ;
  Alcotest.check Alcotest.int "compare 0 0 is 0"
    0 Uint32.(compare zero zero) ;
  Alcotest.check Alcotest.int "compare 1 1 is 0"
    0 Uint32.(compare one one) ;
  Alcotest.check Alcotest.int "compare 0 1 is -1"
    (-1) Uint32.(compare zero one) ;
  Alcotest.check Alcotest.int "compare 1 0 is 1"
    1 Uint32.(compare one zero) ;
  Alcotest.check Alcotest.int "compare 0xFFFFFFFF 0 is 1"
    1 Uint32.(compare (of_int 0xFFFFFFFF) zero) ;
  Alcotest.check Alcotest.int "compare 0 0xFFFFFFFF is -1"
    (-1) Uint32.(compare zero (of_int 0xFFFFFFFF)) ;
  Alcotest.check Alcotest.int "compare 0xFFFFFFFF 0xFFFFFFFE is 1"
    1 Uint32.(compare (of_int 0xFFFFFFFF) (of_int 0xFFFFFFFE)) ;
  Alcotest.check Alcotest.int "compare 0xFFFFFFFE 0xFFFFFFFF is -1"
    (-1) Uint32.(compare (of_int 0xFFFFFFFE) (of_int 0xFFFFFFFF)) ;
  Alcotest.check Alcotest.int "compare 0x7FFFFFFF 0x80000000 is -1"
    (-1) Uint32.(compare (of_int 0x7FFFFFFF) (of_int 0x80000000)) ;
  Alcotest.check Alcotest.int "compare 0x80000000 0x7FFFFFFF is 1"
    1 Uint32.(compare (of_int 0x80000000) (of_int 0x7FFFFFFF))


let basic_tests = [
  "zero is 0l", `Quick, is_zero ;
  "one is 1l", `Quick, is_one ;
  "random of_int", `Slow, of_int_r ;
  "bounds of_int", `Quick, int_bound ;
  "to/of_int", `Slow, to_of_int ;
  "add_ints", `Slow, add_ints ;
  "add overflows", `Quick, add_int_overflow ;
  "add_wrap wraps", `Quick, add_int_wrap ;
  "sub_ints", `Slow, sub_int ;
  "sub underflows", `Quick, sub_int_underflow ;
  "sub_wrap wraps", `Quick, sub_int_wrap ;
  "compare works", `Quick, compare_works
]

let tests = [
  "Basics", basic_tests
]

let () =
  if Sys.word_size <= 32 then
    Printf.eprintf "supposed to be run on 64 bit archs, expect failures" ;
  Nocrypto_entropy_unix.initialize () ;
  Alcotest.run "Uint32 tests" tests
