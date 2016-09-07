open Usane

module Uint8_test = struct
  let uint8 =
    let module M = struct
      type t = Uint8.t
      let pp = Uint8.pp
      let equal s t = Uint8.compare s t = 0
    end in
    (module M : Alcotest.TESTABLE with type t = M.t)

  let r8 ?a () =
    let bound = match a with
      | None -> 0xFFL
      | Some x -> Int64.of_int x
    in
    Int64.to_int (Random.int64 bound)

  let of_int_r () =
    for _i = 0 to 1000 do
      let r = r8 () in
      Alcotest.check uint8 "random" r (Uint8.of_int r)
    done

  let int_bound () =
    Alcotest.check uint8 "int 0 ok" 0 (Uint8.of_int 0) ;
    Alcotest.check_raises "smaller 0 raises"
      (Invalid_argument "out of range")
      (fun () -> ignore (Uint8.of_int (-1))) ;
    Alcotest.check uint8 "int 2 ^ 8 - 1 ok" 0xFF (Uint8.of_int 0xFF) ;
    Alcotest.check_raises "greater 2 ^ 8 - 1 raises"
      (Invalid_argument "out of range")
      (fun () -> ignore (Uint8.of_int (0x100)))

  let add_ints () =
    for _i = 0 to 1000 do
      let a = r8 () in
      let b = r8 ~a:(0xFF - a) () in
      Alcotest.(check (pair uint8 bool) "add works" (Uint8.of_int (a + b), false)
                  Uint8.(add (of_int a) (of_int b)))
    done

  let add_int_overflow () =
    Alcotest.(check (pair uint8 bool) "add 0xFF 1 overflows"
                (0, true)
                Uint8.(add (of_int 0xFF) 1)) ;
    Alcotest.(check (pair uint8 bool) "succ 0xFF overflows"
                (0, true)
                Uint8.(succ (of_int 0xFF))) ;
    Alcotest.(check (pair uint8 bool) "add 0x80 0x80 overflows"
                (0, true)
                Uint8.(add (of_int 0x80) (of_int 0x80))) ;
    Alcotest.(check (pair uint8 bool) "add 0x80 0x7F no overflow"
                (0xFF, false)
                Uint8.(add (of_int 0x80) (of_int 0x7F)))

  let mul_ints () =
    for _i = 0 to 1000 do
      let a = r8 () in
      let b = r8 ~a:0x40 () in
      let p, r =
        let p = a * b in
        if p > 0xFF then (p land 0xFF, true) else (p, false)
      in
      Alcotest.(check (pair uint8 bool) "mul works" (Uint8.of_int p, r)
                  Uint8.(mul (of_int a) (of_int b)))
    done

  let mul_int_overflow () =
    Alcotest.(check (pair uint8 bool) "mul 0xFF 2 overflows"
                (0xFE, true)
                Uint8.(mul (of_int 0xFF) 2)) ;
    Alcotest.(check (pair uint8 bool) "mul 0x3F 2 no overflow"
                (0x7E, false)
                Uint8.(mul (of_int 0x3F) 2)) ;
    Alcotest.(check (pair uint8 bool) "mul 0x3F 4 no overflow"
                (0xFC, false)
                Uint8.(mul (of_int 0x3F) 4)) ;
    Alcotest.(check (pair uint8 bool) "mul 0x7F 2 no overflow"
                (0xFE, false)
                Uint8.(mul (of_int 0x7F) 2)) ;
    Alcotest.(check (pair uint8 bool) "mul 0x80 2 overflows"
                (0, true)
                Uint8.(mul (of_int 0x80) 2)) ;
    Alcotest.(check (pair uint8 bool) "mul 0x40 4 overflows"
                (0, true)
                Uint8.(mul (of_int 0x40) 4)) ;
    Alcotest.(check (pair uint8 bool) "mul 0x40 2 no overflow"
                (0x80, false)
                Uint8.(mul (of_int 0x40) 2))

  let sub_ints () =
    for _i = 0 to 1000 do
      let a = r8 () in
      if a != 0 then
        let b = r8 ~a () in
        Alcotest.(check (pair uint8 bool) "sub works"
                    (Uint8.of_int (a - b), false)
                    Uint8.(sub (of_int a) (of_int b)))
    done

  let sub_int_underflow () =
    Alcotest.(check (pair uint8 bool) "sub 0 1 underflows"
                (0xFF, true)
                Uint8.(sub 0 1)) ;
    Alcotest.(check (pair uint8 bool) "pred 0 underflows"
                (0xFF, true)
                Uint8.(pred 0)) ;
    Alcotest.(check (pair uint8 bool) "sub 0x80 0x81 underflows"
                (0xFF, true)
                Uint8.(sub (of_int 0x80) (of_int 0x81))) ;
    Alcotest.(check (pair uint8 bool) "sub 0x80 0x7F is 1"
                (1, false)
                Uint8.(sub (of_int 0x80) (of_int 0x7F)))

  let compare_works () =
    Alcotest.check Alcotest.int "compare 0xFF 0xFF is 0"
      0 Uint8.(compare (of_int 0xFF) (of_int 0xFF)) ;
    Alcotest.check Alcotest.int "compare 0 0 is 0"
      0 Uint8.(compare 0 0) ;
    Alcotest.check Alcotest.int "compare 1 1 is 0"
      0 Uint8.(compare 1 1) ;
    Alcotest.check Alcotest.int "compare 0 1 is -1"
      (-1) Uint8.(compare 0 1) ;
    Alcotest.check Alcotest.int "compare 1 0 is 1"
      1 Uint8.(compare 1 0) ;
    Alcotest.check Alcotest.int "compare 0xFF 0 is 1"
      1 Uint8.(compare (of_int 0xFF) 0) ;
    Alcotest.check Alcotest.int "compare 0 0xFF is -1"
      (-1) Uint8.(compare 0 (of_int 0xFF)) ;
    Alcotest.check Alcotest.int "compare 0xFF 0xFE is 1"
      1 Uint8.(compare (of_int 0xFF) (of_int 0xFE)) ;
    Alcotest.check Alcotest.int "compare 0xFE 0xFF is -1"
      (-1) Uint8.(compare (of_int 0xFE) (of_int 0xFF)) ;
    Alcotest.check Alcotest.int "compare 0x7F 0x80 is -1"
      (-1) Uint8.(compare (of_int 0x7F) (of_int 0x80)) ;
    Alcotest.check Alcotest.int "compare 0x80 0x7F is 1"
      1 Uint8.(compare (of_int 0x80) (of_int 0x7F))

  let succ_pred_at_bound () =
    Alcotest.(check (pair uint8 bool) "succ 0x7F is 0x80"
                (0x80, false) Uint8.(succ (of_int 0x7F))) ;
    Alcotest.(check (pair uint8 bool) "succ 0x80 is 0x81"
                (0x81, false) Uint8.(succ (of_int 0x80))) ;
    Alcotest.(check (pair uint8 bool) "pred 0x80 is 0x7F"
                (0x7F, false) Uint8.(pred (of_int 0x80))) ;
    Alcotest.(check (pair uint8 bool) "pred 0x81 is 0x80"
                (0x80, false) Uint8.(pred (of_int 0x81)))

  let tests = [
    "random of_int", `Slow, of_int_r ;
    "bounds of_int", `Quick, int_bound ;
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

module Uint16_test = struct
  let uint16 =
    let module M = struct
      type t = Uint16.t
      let pp = Uint16.pp
      let equal s t = Uint16.compare s t = 0
    end in
    (module M : Alcotest.TESTABLE with type t = M.t)

  let r16 ?a () =
    let bound = match a with
      | None -> 0xFFFFL
      | Some x -> Int64.of_int x
    in
    Int64.to_int (Random.int64 bound)

  let of_int_r () =
    for _i = 0 to 1000 do
      let r = r16 () in
      Alcotest.check uint16 "random" r (Uint16.of_int r)
    done

  let int_bound () =
    Alcotest.check uint16 "int 0 ok" 0 (Uint16.of_int 0) ;
    Alcotest.check_raises "smaller 0 raises"
      (Invalid_argument "out of range")
      (fun () -> ignore (Uint16.of_int (-1))) ;
    Alcotest.check uint16 "int 2 ^ 16 - 1 ok" 0xFFFF (Uint16.of_int 0xFFFF) ;
    Alcotest.check_raises "greater 2 ^ 16 - 1 raises"
      (Invalid_argument "out of range")
      (fun () -> ignore (Uint16.of_int (0x10000)))

  let add_ints () =
    for _i = 0 to 1000 do
      let a = r16 () in
      let b = r16 ~a:(0xFFFF - a) () in
      Alcotest.(check (pair uint16 bool) "add works" (Uint16.of_int (a + b), false)
                  Uint16.(add (of_int a) (of_int b)))
    done

  let add_int_overflow () =
    Alcotest.(check (pair uint16 bool) "add 0xFFFF 1 overflows"
                (0, true)
                Uint16.(add (of_int 0xFFFF) 1)) ;
    Alcotest.(check (pair uint16 bool) "succ 0xFFFF overflows"
                (0, true)
                Uint16.(succ (of_int 0xFFFF))) ;
    Alcotest.(check (pair uint16 bool) "add 0x8000 0x8000 overflows"
                (0, true)
                Uint16.(add (of_int 0x8000) (of_int 0x8000))) ;
    Alcotest.(check (pair uint16 bool) "add 0x8000 0x7FFF no overflow"
                (0xFFFF, false)
                Uint16.(add (of_int 0x8000) (of_int 0x7FFF)))

  let mul_ints () =
    for _i = 0 to 1000 do
      let a = r16 () in
      let b = r16 ~a:0x4000 () in
      let p, r =
        let p = a * b in
        if p > 0xFFFF then (p land 0xFFFF, true) else (p, false)
      in
      Alcotest.(check (pair uint16 bool) "mul works" (Uint16.of_int p, r)
                  Uint16.(mul (of_int a) (of_int b)))
    done

  let mul_int_overflow () =
    Alcotest.(check (pair uint16 bool) "mul 0xFFFF 2 overflows"
                (0xFFFE, true)
                Uint16.(mul (of_int 0xFFFF) 2)) ;
    Alcotest.(check (pair uint16 bool) "mul 0x3FFF 2 no overflow"
                (0x7FFE, false)
                Uint16.(mul (of_int 0x3FFF) 2)) ;
    Alcotest.(check (pair uint16 bool) "mul 0x3FFF 4 no overflow"
                (0xFFFC, false)
                Uint16.(mul (of_int 0x3FFF) 4)) ;
    Alcotest.(check (pair uint16 bool) "mul 0x7FFF 2 no overflow"
                (0xFFFE, false)
                Uint16.(mul (of_int 0x7FFF) 2)) ;
    Alcotest.(check (pair uint16 bool) "mul 0x8000 2 overflows"
                (0, true)
                Uint16.(mul (of_int 0x8000) 2)) ;
    Alcotest.(check (pair uint16 bool) "mul 0x4000 4 overflows"
                (0, true)
                Uint16.(mul (of_int 0x4000) 4)) ;
    Alcotest.(check (pair uint16 bool) "mul 0x4000 2 no overflow"
                (0x8000, false)
                Uint16.(mul (of_int 0x4000) 2))

  let sub_ints () =
    for _i = 0 to 1000 do
      let a = r16 () in
      if a != 0 then
        let b = r16 ~a () in
        Alcotest.(check (pair uint16 bool) "sub works"
                    (Uint16.of_int (a - b), false)
                    Uint16.(sub (of_int a) (of_int b)))
    done

  let sub_int_underflow () =
    Alcotest.(check (pair uint16 bool) "sub 0 1 underflows"
                (0xFFFF, true)
                Uint16.(sub 0 1)) ;
    Alcotest.(check (pair uint16 bool) "pred 0 underflows"
                (0xFFFF, true)
                Uint16.(pred 0)) ;
    Alcotest.(check (pair uint16 bool) "sub 0x8000 0x8001 underflows"
                (0xFFFF, true)
                Uint16.(sub (of_int 0x8000) (of_int 0x8001))) ;
    Alcotest.(check (pair uint16 bool) "sub 0x8000 0x7FFF is 1"
                (1, false)
                Uint16.(sub (of_int 0x8000) (of_int 0x7FFF)))

  let compare_works () =
    Alcotest.check Alcotest.int "compare 0xFFFF 0xFFFF is 0"
      0 Uint16.(compare (of_int 0xFFFF) (of_int 0xFFFF)) ;
    Alcotest.check Alcotest.int "compare 0 0 is 0"
      0 Uint16.(compare 0 0) ;
    Alcotest.check Alcotest.int "compare 1 1 is 0"
      0 Uint16.(compare 1 1) ;
    Alcotest.check Alcotest.int "compare 0 1 is -1"
      (-1) Uint16.(compare 0 1) ;
    Alcotest.check Alcotest.int "compare 1 0 is 1"
      1 Uint16.(compare 1 0) ;
    Alcotest.check Alcotest.int "compare 0xFFFF 0 is 1"
      1 Uint16.(compare (of_int 0xFFFF) 0) ;
    Alcotest.check Alcotest.int "compare 0 0xFFFF is -1"
      (-1) Uint16.(compare 0 (of_int 0xFFFF)) ;
    Alcotest.check Alcotest.int "compare 0xFFFF 0xFFFE is 1"
      1 Uint16.(compare (of_int 0xFFFF) (of_int 0xFFFE)) ;
    Alcotest.check Alcotest.int "compare 0xFFFE 0xFFFF is -1"
      (-1) Uint16.(compare (of_int 0xFFFE) (of_int 0xFFFF)) ;
    Alcotest.check Alcotest.int "compare 0x7FFF 0x8000 is -1"
      (-1) Uint16.(compare (of_int 0x7FFF) (of_int 0x8000)) ;
    Alcotest.check Alcotest.int "compare 0x8000 0x7FFF is 1"
      1 Uint16.(compare (of_int 0x8000) (of_int 0x7FFF))

  let succ_pred_at_bound () =
    Alcotest.(check (pair uint16 bool) "succ 0x7FFF is 0x8000"
                (0x8000, false) Uint16.(succ (of_int 0x7FFF))) ;
    Alcotest.(check (pair uint16 bool) "succ 0x8000 is 0x8001"
                (0x8001, false) Uint16.(succ (of_int 0x8000))) ;
    Alcotest.(check (pair uint16 bool) "pred 0x8000 is 0x7FFF"
                (0x7FFF, false) Uint16.(pred (of_int 0x8000))) ;
    Alcotest.(check (pair uint16 bool) "pred 0x8001 is 0x8000"
                (0x8000, false) Uint16.(pred (of_int 0x8001)))

  let tests = [
    "random of_int", `Slow, of_int_r ;
    "bounds of_int", `Quick, int_bound ;
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
      if a != 0 then
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
      if a != 0 then
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
  "Uint8 tests", Uint8_test.tests ;
  "Uint16 tests", Uint16_test.tests ;
  "Uint32 tests", Uint32_test.tests ;
  "Uint64 tests", Uint64_test.tests
]

let () =
  if Sys.word_size <= 32 then
    Printf.eprintf "supposed to be run on 64 bit archs, expect failures" ;
  Random.self_init () ;
  Alcotest.run "Uint tests" tests
