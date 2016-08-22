module Uint32 = struct

  type t = int32

  external add_overflow : t -> t -> t * bool = "caml_uint32_add_overflow"
  external sub_underflow : t -> t -> t * bool = "caml_uint32_sub_underflow"

  let pp ppf t = Format.fprintf ppf "0x%lX" t

  let zero = 0l

  let one = 1l

  let of_int i =
    if i < 0 then
      invalid_arg "out of range"
    else if i > 0xFFFFFFFF then
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

  let add a b = add_overflow a b

  let sub a b = sub_underflow a b

  let pred t = sub t one

  let succ t = add t one

  let compare a b =
    match sub a b with
    | _, true -> -1
    | 0l, _ -> 0
    | _, false -> 1
end


