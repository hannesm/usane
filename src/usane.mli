(** Sane unsigned integers. *)

(** Unsigned 32 bit integers, ranging from 0 to 4294967295 (2 ^ 32 - 1). *)
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

  (** [to_int t] is the integer representation of [t].
      @raise Invalid_argument if out of range (might happen on 32 bit systems). *)
  val to_int : t -> int

  (** [add_exn t t'] is the sum of t and t'.
      @raise Invalid_argument on overflow. *)
  val add_exn : t -> t -> t

  (** [add_wrap t t'] is the sum of t and t'. Wraps on overflow. *)
  val add_wrap : t -> t -> t

  (** [sub_exn t t'] is the result of t - t'.
      @raise Invalid_argument on underflow. *)
  val sub_exn : t -> t -> t

  (** [sub_wrap t t'] is the result of t - t'. Wraps on underflow. *)
  val sub_wrap : t -> t -> t

  (** [succ t] is the successor of [t]: [add_exn t one].
      @raise Invalid_argument if [t] is [2 ^ 32 - 1] (overflow). *)
  val succ : t -> t

  (** [pred t] is the predecessor of [t]: [sub_exn t one].
      @raise Invalid_argument if [t] is [zero] (underflow). *)
  val pred : t -> t

  (** [compare t t'] is -1 if [t] is smaller than [t'],
      0 if [t] and [t'] are equal,
      1 if [t] is greater than [t']. *)
  val compare : t -> t -> int
end


