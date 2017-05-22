(** Sane unsigned integers.

    Arithmetic operations on unsigned integers of fixed width may overflow.
    This library exposes the carry bit of arithmetic operations explicitly: The
    value of each arithmetic operation is a tuple of the (maybe wrapped) value,
    and the carry bit.

    It is designed for network protocols where sequence numbers are used (and
    either overflows are accepted or should lead to re-establishing or re-keying
    of the connection).  Most protocols only use simple arithmetic operations,
    such as a comparison and incrementing by one.

    To ease interoperation with other libraries, the representation is the same
    width signed representation provided by OCaml, if present (which is the case
    for [int32] and [int64]).

    The implementation of [usane] uses C compiler builtins
    ([__builtin_uadd_overflow] etc.).  These are available since GCC 5.1 and
    Clang 3.4.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** Unsigned 8 bit integers, ranging from 0 to 2 ^ 8 - 1 (255). *)
module Uint8 : sig

  (** Type of an unsigned 8 bit integer.  It is represented as an [int]*)
  type t = private int

  (** [pp ppf u] prints the unsigned 8bit integer in hex encoding. *)
  val pp : Format.formatter -> t -> unit

  (** [of_int i] is the integer [i] converted to an unsigned 8 bit integer.
      @raise Invalid_argument if the integer is out of range. *)
  val of_int : int -> t

  (** [add t t'] is [(r, carry)], where [r] is [t + t' mod (2 ^ 8 - 1)].  If
      the sum does not fit into 8 bits, [carry] is [true], otherwise it is
      [false]. *)
  val add : t -> t -> t * bool

  (** [mul t t'] is [(r, carry)], where [r] is [t * t' mod (2 ^ 8 - 1)].  If
      the product does not fit into 8 bits, [carry] is [true], otherwise it is
      [false]. *)
  val mul : t -> t -> t * bool

  (** [sub t t'] is [(r, carry)], where [r] is [t - t' mod (2 ^ 8 - 1)].  If
      [t] is smaller than [t'], [carry] is [true], otherwise it is [false]. *)
  val sub : t -> t -> t * bool

  (** [succ t] is the successor of [t]: {!add} [t 1l].  If [t] is [2 ^ 8 - 1],
      [carry] is [true], otherwise it is [false]. *)
  val succ : t -> t * bool

  (** [pred t] is the predecessor of [t]: {!sub} [t 1l].  If [t] is [0], [carry]
      is [true], otherwise it is [false]. *)
  val pred : t -> t * bool

  (** [zero] is [Uint8.of_int 0] *)
  val zero : t

  (** [compare t t'] is
      {ul
      {- [-1] if [t] is smaller than [t'],}
      {- [0] if [t] and [t'] are equal,}
      {- [1] if [t] is greater than [t'].}} *)
  val compare : t -> t -> int

  (** Convenience infix operator for {!add}. *)
  val ( + ) : t -> t -> t * bool

  (** Convenience infix operator for {!sub}. *)
  val ( - ) : t -> t -> t * bool

  (** Convenience infix operator for {!mul}. *)
  val ( * ) : t -> t -> t * bool

  (** Convenience infix operator for [a < b] using {!compare}. *)
  val ( < )  : t -> t -> bool

  (** Convenience infix operator for [a <= b] using {!compare}. *)
  val ( <= ) : t -> t -> bool

  (** Convenience infix operator for [a > b] using {!compare}. *)
  val ( > )  : t -> t -> bool

  (** Convenience infix operator for [a >= b] using {!compare}. *)
  val ( >= ) : t -> t -> bool
end

(** Unsigned 16 bit integers, ranging from 0 to 2 ^ 16 - 1 (65535). *)
module Uint16 : sig

  (** Type of an unsigned 16 bit integer.  It is represented as an [int]*)
  type t = private int

  (** [pp ppf u] prints the unsigned 16bit integer in hex encoding. *)
  val pp : Format.formatter -> t -> unit

  (** [of_int i] is the integer [i] converted to an unsigned 16 bit integer.
      @raise Invalid_argument if the integer is out of range. *)
  val of_int : int -> t

  (** [add t t'] is [(r, carry)], where [r] is [t + t' mod (2 ^ 16 - 1)].  If
      the sum does not fit into 16 bits, [carry] is [true], otherwise it is
      [false]. *)
  val add : t -> t -> t * bool

  (** [mul t t'] is [(r, carry)], where [r] is [t * t' mod (2 ^ 16 - 1)].  If
      the product does not fit into 16 bits, [carry] is [true], otherwise it is
      [false]. *)
  val mul : t -> t -> t * bool

  (** [sub t t'] is [(r, carry)], where [r] is [t - t' mod (2 ^ 16 - 1)].  If
      [t] is smaller than [t'], [carry] is [true], otherwise it is [false]. *)
  val sub : t -> t -> t * bool

  (** [succ t] is the successor of [t]: {!add} [t 1l].  If [t] is [2 ^ 16 - 1],
      [carry] is [true], otherwise it is [false]. *)
  val succ : t -> t * bool

  (** [pred t] is the predecessor of [t]: {!sub} [t 1l].  If [t] is [0], [carry]
      is [true], otherwise it is [false]. *)
  val pred : t -> t * bool

  (** [zero] is [Uint16.of_int 0] *)
  val zero : t

  (** [compare t t'] is
      {ul
      {- [-1] if [t] is smaller than [t'],}
      {- [0] if [t] and [t'] are equal,}
      {- [1] if [t] is greater than [t'].}} *)
  val compare : t -> t -> int

  (** Convenience infix operator for {!add}. *)
  val ( + ) : t -> t -> t * bool

  (** Convenience infix operator for {!sub}. *)
  val ( - ) : t -> t -> t * bool

  (** Convenience infix operator for {!mul}. *)
  val ( * ) : t -> t -> t * bool

  (** Convenience infix operator for [a < b] using {!compare}. *)
  val ( < )  : t -> t -> bool

  (** Convenience infix operator for [a <= b] using {!compare}. *)
  val ( <= ) : t -> t -> bool

  (** Convenience infix operator for [a > b] using {!compare}. *)
  val ( > )  : t -> t -> bool

  (** Convenience infix operator for [a >= b] using {!compare}. *)
  val ( >= ) : t -> t -> bool
end

(** Unsigned 32 bit integers, ranging from 0 to 2 ^ 32 - 1 (4294967295). *)
module Uint32 : sig

  (** Type of an unsigned 32 bit integer.  It is represented as an [int32]*)
  type t = int32

  (** [pp ppf u] prints the unsigned 32bit integer in hex encoding. *)
  val pp : Format.formatter -> t -> unit

  (** [of_int i] is the integer [i] converted to an unsigned 32 bit integer.
      @raise Invalid_argument if the integer is out of range. *)
  val of_int : int -> t

  (** [to_int t] is the integer representation of [t], encapsulated in [Some].
      If the value [t] does not fit on the host system (if it is 32 bit or
      smaller), it is [None]. *)
  val to_int : t -> int option

  (** [add t t'] is [(r, carry)], where [r] is [t + t' mod (2 ^ 32 - 1)].  If
      the sum does not fit into 32 bits, [carry] is [true], otherwise it is
      [false]. *)
  val add : t -> t -> t * bool

  (** [mul t t'] is [(r, carry)], where [r] is [t * t' mod (2 ^ 32 - 1)].  If
      the product does not fit into 32 bits, [carry] is [true], otherwise it is
      [false]. *)
  val mul : t -> t -> t * bool

  (** [sub t t'] is [(r, carry)], where [r] is [t - t' mod (2 ^ 32 - 1)].  If
      [t] is smaller than [t'], [carry] is [true], otherwise it is [false]. *)
  val sub : t -> t -> t * bool

  (** [succ t] is the successor of [t]: {!add} [t 1l].  If [t] is [2 ^ 32 - 1],
      [carry] is [true], otherwise it is [false]. *)
  val succ : t -> t * bool

  (** [pred t] is the predecessor of [t]: {!sub} [t 1l].  If [t] is [0], [carry]
      is [true], otherwise it is [false]. *)
  val pred : t -> t * bool

  (** [zero] is [Uint32.of_int 0] *)
  val zero : t

  (** [compare t t'] is
      {ul
      {- [-1] if [t] is smaller than [t'],}
      {- [0] if [t] and [t'] are equal,}
      {- [1] if [t] is greater than [t'].}} *)
  val compare : t -> t -> int

  (** Convenience infix operator for {!add}. *)
  val ( + ) : t -> t -> t * bool

  (** Convenience infix operator for {!sub}. *)
  val ( - ) : t -> t -> t * bool

  (** Convenience infix operator for {!mul}. *)
  val ( * ) : t -> t -> t * bool

  (** Convenience infix operator for [a < b] using {!compare}. *)
  val ( < )  : t -> t -> bool

  (** Convenience infix operator for [a <= b] using {!compare}. *)
  val ( <= ) : t -> t -> bool

  (** Convenience infix operator for [a > b] using {!compare}. *)
  val ( > )  : t -> t -> bool

  (** Convenience infix operator for [a >= b] using {!compare}. *)
  val ( >= ) : t -> t -> bool
end

(** Unsigned 64 bit integers, ranging from 0 to 2 ^ 64 - 1 (18446744073709551615). *)
module Uint64 : sig

  (** Type of an unsigned 64 bit integer.  It is represented as an [int64]*)
  type t = int64

  (** [pp ppf u] prints the unsigned 64bit integer in hex encoding. *)
  val pp : Format.formatter -> t -> unit

  (** [of_int i] is the integer [i] converted to an unsigned 64 bit integer.
      @raise Invalid_argument if the integer is out of range. *)
  val of_int : int -> t

  (** [to_int t] is the integer representation of [t], encapsulated in [Some].
      If the value [t] does not fit on the host system (if it is 64 bit or
      smaller), it is [None]. *)
  val to_int : t -> int option

  (** [add t t'] is [(r, carry)], where [r] is [t + t' mod (2 ^ 64 - 1)].  If
      the sum does not fit into 64 bits, [carry] is [true], otherwise it is
      [false]. *)
  val add : t -> t -> t * bool

  (** [mul t t'] is [(r, carry)], where [r] is [t * t' mod (2 ^ 64 - 1)].  If
      the product does not fit into 64 bits, [carry] is [true], otherwise it is
      [false]. *)
  val mul : t -> t -> t * bool

  (** [sub t t'] is [(r, carry)], where [r] is [t - t' mod (2 ^ 64 - 1)].  If
      [t] is smaller than [t'], [carry] is [true], otherwise it is [false]. *)
  val sub : t -> t -> t * bool

  (** [succ t] is the successor of [t]: {!add} [t 1l].  If [t] is [2 ^ 64 - 1],
      [carry] is [true], otherwise it is [false]. *)
  val succ : t -> t * bool

  (** [pred t] is the predecessor of [t]: {!sub} [t 1l].  If [t] is [0], [carry]
      is [true], otherwise it is [false]. *)
  val pred : t -> t * bool

  (** [zero] is [Uint64.of_int 0] *)
  val zero : t

  (** [compare t t'] is
      {ul
      {- [-1] if [t] is smaller than [t'],}
      {- [0] if [t] and [t'] are equal,}
      {- [1] if [t] is greater than [t'].}} *)
  val compare : t -> t -> int

  (** Convenience infix operator for {!add}. *)
  val ( + ) : t -> t -> t * bool

  (** Convenience infix operator for {!sub}. *)
  val ( - ) : t -> t -> t * bool

  (** Convenience infix operator for {!mul}. *)
  val ( * ) : t -> t -> t * bool

  (** Convenience infix operator for [a < b] using {!compare}. *)
  val ( < )  : t -> t -> bool

  (** Convenience infix operator for [a <= b] using {!compare}. *)
  val ( <= ) : t -> t -> bool

  (** Convenience infix operator for [a > b] using {!compare}. *)
  val ( > )  : t -> t -> bool

  (** Convenience infix operator for [a >= b] using {!compare}. *)
  val ( >= ) : t -> t -> bool
end
