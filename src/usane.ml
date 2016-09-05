module Uint32 = struct

  type t = int32

  let pp ppf t = Format.fprintf ppf "0x%08lX" t

  let of_int i =
    if i < 0 then
      invalid_arg "out of range"
    else if i > 2 * Int32.(to_int max_int) + 1 then
      invalid_arg "out of range"
    else
      Int32.of_int i

  let to_int t =
    if Sys.word_size <= 32 && t < 0l then
      None
    else if t < 0l then
      Some (Int32.(to_int (add t min_int)) + Int32.(to_int max_int) + 1)
    else
      Some (Int32.to_int t)

  external add : t -> t -> t * bool = "caml_uint32_add_overflow"
  external mul : t -> t -> t * bool = "caml_uint32_mul_overflow"
  external sub : t -> t -> t * bool = "caml_uint32_sub_underflow"

  let pred t = sub t 1l

  let succ t = add t 1l

  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul

  let compare a b =
    match sub a b with
    | _, true -> -1
    | 0l, _ -> 0
    | _, false -> 1

  let ( <  ) a b = compare a b < 0
  let ( <= ) a b = compare a b <= 0
  let ( >  ) a b = compare a b > 0
  let ( >= ) a b = compare a b >= 0
end
