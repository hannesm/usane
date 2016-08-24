open Usane

let uint32 =
  let module M = struct
    type t = Uint32.t
    let pp = Uint32.pp
    let equal s t = Uint32.compare s t = 0
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let r32 ?a () =
  let bound = match a with
    | None -> 0xFFFFFFFFL
    | Some x -> Int64.of_int x
  in
  Int64.to_int (Random.int64 bound)

let of_int_r () =
  for _i = 0 to 1000 do
    let r = r32 () in
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
  for _i = 0 to 1000 do
    let r = r32 () in
    Alcotest.(check (option int) "to_int (of_int x) works" (Some r)
                Uint32.(to_int (of_int r)))
  done

let add_ints () =
  for _i = 0 to 1000 do
    let a = r32 () in
    let b = r32 ~a:(0xFFFFFFFF - a) () in
    Alcotest.(check (pair uint32 bool) "add works" (Uint32.of_int (a + b), false)
                Uint32.(add (of_int a) (of_int b)))
  done

let add_int_overflow () =
  Alcotest.(check (pair uint32 bool) "add 0xFFFFFFFF 1 wraps"
              (0l, true)
              Uint32.(add (of_int 0xFFFFFFFF) 1l)) ;
  Alcotest.(check (pair uint32 bool) "succ 0xFFFFFFFF wraps"
              (0l, true)
              Uint32.(succ (of_int 0xFFFFFFFF))) ;
  Alcotest.(check (pair uint32 bool) "add 0x800000000 0x80000000 wraps"
              (0l, true)
              Uint32.(add (of_int 0x80000000) (of_int 0x80000000))) ;
  Alcotest.(check (pair uint32 bool) "add 0x800000000 0x7FFFFFFF is good"
              (0xFFFFFFFFl, false)
              Uint32.(add (of_int 0x80000000) (of_int 0x7FFFFFFF)))

let mul_ints () =
  for _i = 0 to 1000 do
    let a = r32 () in
    let b = r32 ~a:0x40000000 () in
    let p, r =
      let p = a * b in
      if p > 0xFFFFFFFF then (p land 0xFFFFFFFF, true) else (p, false)
    in
    let tst = Printf.sprintf "a %08X b %08X p %08X r %b" a b p r in
    Alcotest.(check (pair uint32 bool) ("mul works " ^ tst) (Uint32.of_int p, r)
                Uint32.(mul (of_int a) (of_int b)))
  done

let mul_int_overflow () =
  Alcotest.(check (pair uint32 bool) "mul 0xFFFFFFFF 2 wraps"
              (0xFFFFFFFEl, true)
              Uint32.(mul (of_int 0xFFFFFFFF) 2l)) ;
  Alcotest.(check (pair uint32 bool) "mul 0x3FFFFFFF 2 no wrap"
              (0x7FFFFFFEl, false)
              Uint32.(mul (of_int 0x3FFFFFFF) 2l)) ;
  Alcotest.(check (pair uint32 bool) "mul 0x3FFFFFFF 4 no wrap"
              (0xFFFFFFFCl, false)
              Uint32.(mul (of_int 0x3FFFFFFF) 4l)) ;
  Alcotest.(check (pair uint32 bool) "mul 0x7FFFFFFF 2 no wrap"
              (0xFFFFFFFEl, false)
              Uint32.(mul (of_int 0x7FFFFFFF) 2l)) ;
  Alcotest.(check (pair uint32 bool) "mul 0x80000000 2 wrap"
              (0l, true)
              Uint32.(mul (of_int 0x80000000) 2l)) ;
  Alcotest.(check (pair uint32 bool) "mul 0x40000000 4 wrap"
              (0l, true)
              Uint32.(mul (of_int 0x40000000) 4l)) ;
  Alcotest.(check (pair uint32 bool) "mul 0x40000000 2 no wrap"
              (0x80000000l, false)
              Uint32.(mul (of_int 0x40000000) 2l))

let sub_ints () =
  for _i = 0 to 1000 do
    let a = r32 () in
    let b = r32 ~a () in
    Alcotest.(check (pair uint32 bool) "sub works"
                (Uint32.of_int (a - b), false)
                Uint32.(sub (of_int a) (of_int b)))
  done

let sub_int_underflow () =
  Alcotest.(check (pair uint32 bool) "sub 0 1 wraps"
              (0xFFFFFFFFl, true)
              Uint32.(sub 0l 1l)) ;
  Alcotest.(check (pair uint32 bool) "pred 0 wraps"
              (0xFFFFFFFFl, true)
              Uint32.(pred 0l)) ;
  Alcotest.(check (pair uint32 bool) "sub 0x800000000 0x80000001 wraps"
              (0xFFFFFFFFl, true)
              Uint32.(sub (of_int 0x80000000) (of_int 0x80000001))) ;
  Alcotest.(check (pair uint32 bool) "sub 0x800000000 0x7FFFFFFF is 1"
              (1l, false)
              Uint32.(sub (of_int 0x80000000) (of_int 0x7FFFFFFF)))

let compare_works () =
  Alcotest.check Alcotest.int "compare 0xFFFFFFFF 0xFFFFFFFF is 0"
    0 Uint32.(compare (of_int 0xFFFFFFFF) (of_int 0xFFFFFFFF)) ;
  Alcotest.check Alcotest.int "compare 0 0 is 0"
    0 Uint32.(compare 0l 0l) ;
  Alcotest.check Alcotest.int "compare 1 1 is 0"
    0 Uint32.(compare 1l 1l) ;
  Alcotest.check Alcotest.int "compare 0 1 is -1"
    (-1) Uint32.(compare 0l 1l) ;
  Alcotest.check Alcotest.int "compare 1 0 is 1"
    1 Uint32.(compare 1l 0l) ;
  Alcotest.check Alcotest.int "compare 0xFFFFFFFF 0 is 1"
    1 Uint32.(compare (of_int 0xFFFFFFFF) 0l) ;
  Alcotest.check Alcotest.int "compare 0 0xFFFFFFFF is -1"
    (-1) Uint32.(compare 0l (of_int 0xFFFFFFFF)) ;
  Alcotest.check Alcotest.int "compare 0xFFFFFFFF 0xFFFFFFFE is 1"
    1 Uint32.(compare (of_int 0xFFFFFFFF) (of_int 0xFFFFFFFE)) ;
  Alcotest.check Alcotest.int "compare 0xFFFFFFFE 0xFFFFFFFF is -1"
    (-1) Uint32.(compare (of_int 0xFFFFFFFE) (of_int 0xFFFFFFFF)) ;
  Alcotest.check Alcotest.int "compare 0x7FFFFFFF 0x80000000 is -1"
    (-1) Uint32.(compare (of_int 0x7FFFFFFF) (of_int 0x80000000)) ;
  Alcotest.check Alcotest.int "compare 0x80000000 0x7FFFFFFF is 1"
    1 Uint32.(compare (of_int 0x80000000) (of_int 0x7FFFFFFF))

let succ_pred_at_bound () =
  Alcotest.(check (pair uint32 bool) "succ 0x7FFFFFFF is 0x80000000"
              (0x80000000l, false) Uint32.(succ (of_int 0x7FFFFFFF))) ;
  Alcotest.(check (pair uint32 bool) "succ 0x80000000 is 0x80000001"
              (0x80000001l, false) Uint32.(succ (of_int 0x80000000))) ;
  Alcotest.(check (pair uint32 bool) "pred 0x80000000 is 0x7FFFFFFF"
              (0x7FFFFFFFl, false) Uint32.(pred (of_int 0x80000000))) ;
  Alcotest.(check (pair uint32 bool) "pred 0x80000001 is 0x80000000"
              (0x80000000l, false) Uint32.(pred (of_int 0x80000001)))

let basic_tests = [
  "random of_int", `Slow, of_int_r ;
  "bounds of_int", `Quick, int_bound ;
  "to/of_int", `Slow, to_of_int ;
  "add", `Slow, add_ints ;
  "add overflows", `Quick, add_int_overflow ;
  "mul", `Slow, mul_ints ;
  "mul overflows", `Quick, mul_int_overflow ;
  "sub", `Slow, sub_ints ;
  "sub underflows", `Quick, sub_int_underflow ;
  "compare works", `Quick, compare_works ;
  "succ/pred works", `Quick, succ_pred_at_bound
]

let tests = [
  "Basics", basic_tests
]

let () =
  if Sys.word_size <= 32 then
    Printf.eprintf "supposed to be run on 64 bit archs, expect failures" ;
  Random.self_init () ;
  Alcotest.run "Uint32 tests" tests
