open Core.Std

module Fract : sig
  type t
end

val make : int -> int -> Fract.t
val to_string : Fract.t -> string
val add : Fract.t -> Fract.t -> Fract.t
val of_int : int -> Fract.t
val mul : Fract.t -> Fract.t -> Fract.t
val mul_int : Fract.t -> int -> Fract.t
