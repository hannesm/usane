module Uint32 = struct

  type t = int32

  external add_overflow : t -> t -> t = "caml_uint32_add_overflow"
  external sub_underflow : t -> t -> t = "caml_uint32_sub_underflow"

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
      invalid_arg "out of range"
    else if t < 0l then
      Int32.(to_int (add t min_int)) + Int32.(to_int max_int) + 1
    else
      Int32.to_int t

  let add_exn a b = add_overflow a b

  let add_wrap a b = Int32.add a b

  let sub_exn a b = sub_underflow a b

  let sub_wrap a b = Int32.sub a b

  let pred t = sub_exn t one

  let succ t = add_exn t one

  let compare a b =
    try if sub_exn a b = 0l then 0 else 1
    with Invalid_argument _ -> -1
end


