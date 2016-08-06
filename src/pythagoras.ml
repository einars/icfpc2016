open Core.Std

open Facets

module F = Fractions

let debug = true

let triangle () = 

  let n = Random.int 5 in
  let n = n * 2 + 1 in (* odd *)
  let o = n + 2 in
  let t1 = n + o in
  let t2 = n * o in
  let t3 = (t1 * t1 + t2 * t2) |> float_of_int |> sqrt |> int_of_float in
  if t1 * t1 + t2 * t2 <> t3 * t3 then
    failwithf "I suck at making triangles, I tried %d %d %d (base %d)\n" t1 t2 t3 n ();

  F.of_int t1, F.of_int t2, F.of_int t3
;;


let pythamorph facets =
  let t1, t2, t3 = triangle () in
  let p_sin = F.div t1 t3
  and p_cos = F.div t2 t3 in

  if debug then eprintf "Pythamorph %s %s %s\n%!"
    (F.to_s t1) (F.to_s t2) (F.to_s t3);

  let rotate_point p = 
    let tgt = {
      x = (F.sub (F.mul p.x p_cos) (F.mul p.y p_sin));
      y = (F.add (F.mul p.x p_sin) (F.mul p.y p_cos));
    } in
    tgt
  
  in

  let rotate_plane_point (p:Facets.plane_point_t) = {
    actual = rotate_point p.actual;
    original = p.original;
  } in

  let rotate_facet facet = 
    List.map ~f:rotate_plane_point facet
  in

  List.map ~f:rotate_facet facets
;;
    
