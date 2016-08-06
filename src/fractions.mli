open Core.Std

module Fract : sig

  type t

  (* boxed instead of int*int so that I wouldn't make unnormalized fractions
  * manually by mistake  *)

end


(* returns prev. version *)
val with_human_output : (unit -> unit) -> unit

val make : int -> int -> Fract.t
val to_s : Fract.t -> string
val of_int : int -> Fract.t
val add : Fract.t -> Fract.t -> Fract.t
val sub : Fract.t -> Fract.t -> Fract.t
val mul : Fract.t -> Fract.t -> Fract.t
val mul_int : Fract.t -> int -> Fract.t
val div : Fract.t -> Fract.t -> Fract.t
val div_int : Fract.t -> int -> Fract.t
val is_zero: Fract.t -> bool
val square: Fract.t -> Fract.t

val eq: Fract.t -> Fract.t -> bool
val gt: Fract.t -> Fract.t -> bool
val lt: Fract.t -> Fract.t -> bool
val gte: Fract.t -> Fract.t -> bool
val lte: Fract.t -> Fract.t -> bool

val max : Fract.t -> Fract.t -> Fract.t
val min : Fract.t -> Fract.t -> Fract.t

val zero : Fract.t
val one : Fract.t


