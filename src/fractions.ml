
module P = Printf

open Big_int

module Fract = struct
  type t = big_int * big_int
end

let rec normalize (n,d) =
  if lt_big_int d zero_big_int then normalize (minus_big_int n, minus_big_int d)
  else
    let g = gcd_big_int n d in
    if eq_big_int g unit_big_int then (n,d)
    else let nn,dd = (div_big_int n g, div_big_int d g) in
    if lt_big_int dd zero_big_int then (minus_big_int nn, minus_big_int dd) else (nn, dd)
;;

let make n d = normalize (big_int_of_int n, big_int_of_int d)
;;


type display_mode_t = Fractional | Decimal

let display_mode = ref Fractional

let to_s (n,d) = 
  if !display_mode = Decimal then P.sprintf "%.02f" ((float_of_big_int n) /. (float_of_big_int d))
  else if eq_big_int n zero_big_int then "0"
  else if eq_big_int d unit_big_int then string_of_big_int n
  else P.sprintf "%s/%s" (string_of_big_int n) (string_of_big_int d)
;;

let of_int n =
  big_int_of_int n, unit_big_int
;;

let add (n1, d1) (n2, d2) =
  normalize (add_big_int (mult_big_int n1 d2) (mult_big_int n2 d1), (mult_big_int d1 d2))
;;

let sub (n1, d1) (n2, d2) =
  normalize (sub_big_int (mult_big_int n1 d2) (mult_big_int n2 d1), (mult_big_int d1 d2))
;;

let mul (n1, d1) (n2, d2) =
  normalize ((mult_big_int n1 n2), (mult_big_int d1 d2))
;;

let mul_int (n, d) i =
  normalize ((mult_int_big_int i n), d)
;;

let div (n1, d1) (n2, d2) =
  normalize ((mult_big_int n1 d2), (mult_big_int d1 n2))
;;


let div_int (n, d) i =
  normalize (n, (mult_int_big_int i d))
;;

let square (n, d) = normalize ((mult_big_int n n), (mult_big_int d d))
;;


let is_zero (n, _) = eq_big_int n zero_big_int

let eq (n1, d1) (n2, d2) = (eq_big_int n1 n2) && (eq_big_int d1 d2)
let gt (n1, d1) (n2, d2) = gt_big_int (mult_big_int n1 d2) (mult_big_int n2 d1)
let lt (n1, d1) (n2, d2) = lt_big_int (mult_big_int n1 d2) (mult_big_int n2 d1)
let gte (n1, d1) (n2, d2) = ge_big_int (mult_big_int n1 d2) (mult_big_int n2 d1)
let lte (n1, d1) (n2, d2) = le_big_int (mult_big_int n1 d2) (mult_big_int n2 d1)

let min p1 p2 = if gt p1 p2 then p2 else p1
let max p1 p2 = if gt p1 p2 then p1 else p2

let zero = zero_big_int, unit_big_int
let one = unit_big_int, unit_big_int
let minus_one = (big_int_of_int (-1)), unit_big_int

let with_human_output fn =
  display_mode := Decimal;
  fn();
  display_mode := Fractional;
;;
