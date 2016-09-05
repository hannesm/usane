open Usane

module Uint32_test = struct
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
    Alcotest.(check (pair uint32 bool) "add 0xFFFFFFFF 1 overflows"
                (0l, true)
                Uint32.(add (of_int 0xFFFFFFFF) 1l)) ;
    Alcotest.(check (pair uint32 bool) "succ 0xFFFFFFFF overflows"
                (0l, true)
                Uint32.(succ (of_int 0xFFFFFFFF))) ;
    Alcotest.(check (pair uint32 bool) "add 0x800000000 0x80000000 overflows"
                (0l, true)
                Uint32.(add (of_int 0x80000000) (of_int 0x80000000))) ;
    Alcotest.(check (pair uint32 bool) "add 0x800000000 0x7FFFFFFF no overflow"
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
      Alcotest.(check (pair uint32 bool) "mul works" (Uint32.of_int p, r)
                  Uint32.(mul (of_int a) (of_int b)))
    done

  let mul_int_overflow () =
    Alcotest.(check (pair uint32 bool) "mul 0xFFFFFFFF 2 overflows"
                (0xFFFFFFFEl, true)
                Uint32.(mul (of_int 0xFFFFFFFF) 2l)) ;
    Alcotest.(check (pair uint32 bool) "mul 0x3FFFFFFF 2 no overflow"
                (0x7FFFFFFEl, false)
                Uint32.(mul (of_int 0x3FFFFFFF) 2l)) ;
    Alcotest.(check (pair uint32 bool) "mul 0x3FFFFFFF 4 no overflow"
                (0xFFFFFFFCl, false)
                Uint32.(mul (of_int 0x3FFFFFFF) 4l)) ;
    Alcotest.(check (pair uint32 bool) "mul 0x7FFFFFFF 2 no overflow"
                (0xFFFFFFFEl, false)
                Uint32.(mul (of_int 0x7FFFFFFF) 2l)) ;
    Alcotest.(check (pair uint32 bool) "mul 0x80000000 2 overflows"
                (0l, true)
                Uint32.(mul (of_int 0x80000000) 2l)) ;
    Alcotest.(check (pair uint32 bool) "mul 0x40000000 4 overflows"
                (0l, true)
                Uint32.(mul (of_int 0x40000000) 4l)) ;
    Alcotest.(check (pair uint32 bool) "mul 0x40000000 2 no overflow"
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
    Alcotest.(check (pair uint32 bool) "sub 0 1 underflows"
                (0xFFFFFFFFl, true)
                Uint32.(sub 0l 1l)) ;
    Alcotest.(check (pair uint32 bool) "pred 0 underflows"
                (0xFFFFFFFFl, true)
                Uint32.(pred 0l)) ;
    Alcotest.(check (pair uint32 bool) "sub 0x800000000 0x80000001 underflows"
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

  let tests = [
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
end

module Uint64_test = struct
  let uint64 =
    let module M = struct
      type t = Uint64.t
      let pp = Uint64.pp
      let equal s t = Uint64.compare s t = 0
    end in
    (module M : Alcotest.TESTABLE with type t = M.t)

  let r64 ?a () =
    let bound = match a with
      | None -> Int64.of_int max_int
      | Some x -> Int64.of_int x
    in
    Int64.to_int (Random.int64 bound)

  let of_int_r () =
    for _i = 0 to 1000 do
      let r = r64 () in
      Alcotest.check uint64 "random" (Int64.of_int r) (Uint64.of_int r)
    done

  let int_bound () =
    Alcotest.check uint64 "int 0 ok" 0L (Uint64.of_int 0) ;
    Alcotest.check_raises "smaller 0 raises"
      (Invalid_argument "out of range")
      (fun () -> ignore (Uint64.of_int (-1)))

  let to_of_int () =
    for _i = 0 to 1000 do
      let r = r64 () in
      Alcotest.(check (option int) "to_int (of_int x) works" (Some r)
                  Uint64.(to_int (of_int r)))
    done

  let add_ints () =
    for _i = 0 to 1000 do
      let a = r64 () in
      let b = r64 ~a:(max_int - a) () in
      Alcotest.(check (pair uint64 bool) "add works" (Uint64.of_int (a + b), false)
                  Uint64.(add (of_int a) (of_int b)))
    done

  let add_int_overflow () =
    Alcotest.(check (pair uint64 bool) "add 0xFFFFFFFFFFFFFFFF 1 overflows"
                (0L, true)
                Uint64.(add 0xFFFFFFFFFFFFFFFFL 1L)) ;
    Alcotest.(check (pair uint64 bool) "succ 0xFFFFFFFFFFFFFFFF overflows"
                (0L, true)
                Uint64.(succ 0xFFFFFFFFFFFFFFFFL)) ;
    Alcotest.(check (pair uint64 bool) "add 0x80000000000000000 0x8000000000000000 overflows"
                (0L, true)
                Uint64.(add 0x8000000000000000L 0x8000000000000000L)) ;
    Alcotest.(check (pair uint64 bool) "add 0x80000000000000000 0x7FFFFFFFFFFFFFFF no overflow"
                (0xFFFFFFFFFFFFFFFFL, false)
                Uint64.(add 0x8000000000000000L 0x7FFFFFFFFFFFFFFFL))
(*
  let mul_ints () =
    for _i = 0 to 1000 do
      let a = r64 () in
      let b = r64 ~a:0x4000000000000000 () in
      let p, r =
        let p = a * b in
        if p > 0xFFFFFFFFFFFFFFFF then (p land 0xFFFFFFFFFFFFFFFF, true) else (p, false)
      in
      Alcotest.(check (pair uint64 bool) "mul works" (Uint64.of_int p, r)
                  Uint64.(mul (of_int a) (of_int b)))
    done
*)

  let mul_int_overflow () =
    Alcotest.(check (pair uint64 bool) "mul 0xFFFFFFFFFFFFFFFF 2 overflows"
                (0xFFFFFFFFFFFFFFFEL, true)
                Uint64.(mul 0xFFFFFFFFFFFFFFFFL 2L)) ;
    Alcotest.(check (pair uint64 bool) "mul 0x3FFFFFFFFFFFFFFF 2 no overflow"
                (0x7FFFFFFFFFFFFFFEL, false)
                Uint64.(mul 0x3FFFFFFFFFFFFFFFL 2L)) ;
    Alcotest.(check (pair uint64 bool) "mul 0x3FFFFFFFFFFFFFFF 4 no overflow"
                (0xFFFFFFFFFFFFFFFCL, false)
                Uint64.(mul 0x3FFFFFFFFFFFFFFFL 4L)) ;
    Alcotest.(check (pair uint64 bool) "mul 0x7FFFFFFFFFFFFFFF 2 no overflow"
                (0xFFFFFFFFFFFFFFFEL, false)
                Uint64.(mul 0x7FFFFFFFFFFFFFFFL 2L)) ;
    Alcotest.(check (pair uint64 bool) "mul 0x8000000000000000 2 overflows"
                (0L, true)
                Uint64.(mul 0x8000000000000000L 2L)) ;
    Alcotest.(check (pair uint64 bool) "mul 0x4000000000000000 4 overflows"
                (0L, true)
                Uint64.(mul 0x4000000000000000L 4L)) ;
    Alcotest.(check (pair uint64 bool) "mul 0x4000000000000000 2 no overflow"
                (0x8000000000000000L, false)
                Uint64.(mul 0x4000000000000000L 2L))

  let sub_ints () =
    for _i = 0 to 1000 do
      let a = r64 () in
      let b = r64 ~a () in
      Alcotest.(check (pair uint64 bool) "sub works"
                  (Uint64.of_int (a - b), false)
                  Uint64.(sub (of_int a) (of_int b)))
    done

  let sub_int_underflow () =
    Alcotest.(check (pair uint64 bool) "sub 0 1 underflows"
                (0xFFFFFFFFFFFFFFFFL, true)
                Uint64.(sub 0L 1L)) ;
    Alcotest.(check (pair uint64 bool) "pred 0 underflows"
                (0xFFFFFFFFFFFFFFFFL, true)
                Uint64.(pred 0L)) ;
    Alcotest.(check (pair uint64 bool) "sub 0x80000000000000000 0x8000000000000001 underflows"
                (0xFFFFFFFFFFFFFFFFL, true)
                Uint64.(sub 0x8000000000000000L 0x8000000000000001L)) ;
    Alcotest.(check (pair uint64 bool) "sub 0x80000000000000000 0x7FFFFFFFFFFFFFFF is 1"
                (1L, false)
                Uint64.(sub 0x8000000000000000L 0x7FFFFFFFFFFFFFFFL))

  let compare_works () =
    Alcotest.check Alcotest.int "compare 0xFFFFFFFFFFFFFFFF 0xFFFFFFFFFFFFFFFF is 0"
      0 Uint64.(compare 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFFL) ;
    Alcotest.check Alcotest.int "compare 0 0 is 0"
      0 Uint64.(compare 0L 0L) ;
    Alcotest.check Alcotest.int "compare 1 1 is 0"
      0 Uint64.(compare 1L 1L) ;
    Alcotest.check Alcotest.int "compare 0 1 is -1"
      (-1) Uint64.(compare 0L 1L) ;
    Alcotest.check Alcotest.int "compare 1 0 is 1"
      1 Uint64.(compare 1L 0L) ;
    Alcotest.check Alcotest.int "compare 0xFFFFFFFFFFFFFFFF 0 is 1"
      1 Uint64.(compare 0xFFFFFFFFFFFFFFFFL 0L) ;
    Alcotest.check Alcotest.int "compare 0 0xFFFFFFFFFFFFFFFF is -1"
      (-1) Uint64.(compare 0L 0xFFFFFFFFFFFFFFFFL) ;
    Alcotest.check Alcotest.int "compare 0xFFFFFFFFFFFFFFFF 0xFFFFFFFFFFFFFFFE is 1"
      1 Uint64.(compare 0xFFFFFFFFFFFFFFFFL 0xFFFFFFFFFFFFFFFEL) ;
    Alcotest.check Alcotest.int "compare 0xFFFFFFFFFFFFFFFE 0xFFFFFFFFFFFFFFFF is -1"
      (-1) Uint64.(compare 0xFFFFFFFFFFFFFFFEL 0xFFFFFFFFFFFFFFFFL) ;
    Alcotest.check Alcotest.int "compare 0x7FFFFFFFFFFFFFFF 0x8000000000000000 is -1"
      (-1) Uint64.(compare 0x7FFFFFFFFFFFFFFFL 0x8000000000000000L) ;
    Alcotest.check Alcotest.int "compare 0x8000000000000000 0x7FFFFFFFFFFFFFFF is 1"
      1 Uint64.(compare 0x8000000000000000L 0x7FFFFFFFFFFFFFFFL)

  let succ_pred_at_bound () =
    Alcotest.(check (pair uint64 bool) "succ 0x7FFFFFFFFFFFFFFF is 0x8000000000000000"
                (0x8000000000000000L, false) Uint64.(succ 0x7FFFFFFFFFFFFFFFL)) ;
    Alcotest.(check (pair uint64 bool) "succ 0x8000000000000000 is 0x8000000000000001"
                (0x8000000000000001L, false) Uint64.(succ 0x8000000000000000L)) ;
    Alcotest.(check (pair uint64 bool) "pred 0x8000000000000000 is 0x7FFFFFFFFFFFFFFF"
                (0x7FFFFFFFFFFFFFFFL, false) Uint64.(pred 0x8000000000000000L)) ;
    Alcotest.(check (pair uint64 bool) "pred 0x8000000000000001 is 0x8000000000000000"
                (0x8000000000000000L, false) Uint64.(pred 0x8000000000000001L))

  let tests = [
    "random of_int", `Slow, of_int_r ;
    "bounds of_int", `Quick, int_bound ;
    "to/of_int", `Slow, to_of_int ;
    "add", `Slow, add_ints ;
    "add overflows", `Quick, add_int_overflow ;
    (*    "mul", `Slow, mul_ints ; *)
    "mul overflows", `Quick, mul_int_overflow ;
    "sub", `Slow, sub_ints ;
    "sub underflows", `Quick, sub_int_underflow ;
    "compare works", `Quick, compare_works ;
    "succ/pred works", `Quick, succ_pred_at_bound
  ]
end

let tests = [
  "Uint32 tests", Uint32_test.tests ;
  "Uint64 tests", Uint64_test.tests
]

let () =
  if Sys.word_size <= 32 then
    Printf.eprintf "supposed to be run on 64 bit archs, expect failures" ;
  Random.self_init () ;
  Alcotest.run "Uint tests" tests
