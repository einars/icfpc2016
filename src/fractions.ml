
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
    else (n / g, d / g)
;;

let make n d = normalize (n, d)
;;

let to_string (n,d) = 
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

let mul (n1, d1) (n2, d2) =
  normalize (n1 * n2, d1 * d2)
;;

let mul_int (n, d) i =
  normalize (n * i, d)
;;

