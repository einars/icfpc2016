
module P = Printf

module Fract = struct
  type t = int * int
end

let rec gcd a b =
  if b = 0 then a else gcd b (a mod b)
;;


let rec normalize (n,d) =
  if (d < 0) then normalize (-n, -d)
  else
    let g = gcd n d in
    if g = 1 then (n,d)
    else let nn,dd = (n / g, d / g) in
    if dd < 0 then (-nn, -dd) else (nn, dd)
;;

let make n d = normalize (n, d)
;;

let to_s (n,d) = 
  if n = 0 then "0"
  else if d = 1 then P.sprintf "%d" n
  else P.sprintf "%d/%d" n d
;;

let of_int n =
  n, 1
;;

let add (n1, d1) (n2, d2) =
  normalize (n1 * d2 + n2 * d1, d1 * d2)
;;

let sub (n1, d1) (n2, d2) =
  normalize (n1 * d2 - n2 * d1, d1 * d2)
;;

let mul (n1, d1) (n2, d2) =
  normalize (n1 * n2, d1 * d2)
;;

let mul_int (n, d) i =
  normalize (n * i, d)
;;

let div (n1, d1) (n2, d2) =
  normalize (n1 * d2, d1 * n2)
;;


let div_int (n, d) i =
  normalize (n, d * i)
;;

let square (n, d) = normalize (n * n, d * d)
;;


let is_zero (n, d) = n = 0

let gt (n1, d1) (n2, d2) = n1 * d2 > n2 * d1
let lt (n1, d1) (n2, d2) = n1 * d2 < n2 * d1
let gte (n1, d1) (n2, d2) = n1 * d2 >= n2 * d1
let lte (n1, d1) (n2, d2) = n1 * d2 <= n2 * d1

let min p1 p2 = if gt p1 p2 then p2 else p1
let max p1 p2 = if gt p1 p2 then p1 else p2

let zero = 0,1
let one = 1,1
