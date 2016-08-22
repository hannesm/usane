(** Sane unsigned integers.


    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** Unsigned 32 bit integers, ranging from 0 to 2 ^ 32 - 1 (4294967295). *)
module Uint32 : sig

  (** Type of an unsigned 32 bit integer.  It is represented as an [int32]*)
  type t = int32

  (** [pp ppf u] prints the unsigned 32bit integer. *)
  val pp : Format.formatter -> t -> unit

  (** [zero] is 0. *)
  val zero : t

  (** [one] is 1. *)
  val one : t

  (** [of_int i] is the integer [i] converted to an unsigned 32 bit integer.
      @raise Invalid_argument if the integer is out of range. *)
  val of_int : int -> t

  (** [to_int t] is the integer representation of [t], if it fits, [None]
      otherwise. *)
  val to_int : t -> int option

  (** [add t t'] is [r, wrap], where [r] is the result of [t + t' mod (2 ^ 32 -
      1)].  If the sum does not fit into 32 bits, [wrap] is [true], otherwise
      [false]. *)
  val add : t -> t -> t * bool

  (** [sub t t'] is [r, wrap], where [r] is the result of [t - t' mod (2 ^ 32 -
      1)].  If [t] is smaller than [t'], [wrap] is [true], otherwise [false]. *)
  val sub : t -> t -> t * bool

  (** [succ t] is the successor of [t]: [add t one].  If [t] is 2 ^ 32 - 1,
      [wrap] is [true], otherwise [false]. *)
  val succ : t -> t * bool

  (** [pred t] is the predecessor of [t]: [sub t one].  If [t] is [zero], [wrap]
      is [true], otherwise [false]. *)
  val pred : t -> t * bool

  (** [compare t t'] is
      {ul
      {- [-1] if [t] is smaller than [t'],}
      {- [0] if [t] and [t'] are equal,}
      {- [1] if [t] is greater than [t'].}} *)
  val compare : t -> t -> int
end


