open Core.Std

module F = Fractions
module P = Printf

let debug = true

(* ~~~ begin types ~~~ *)

type point_t = {
  x: Fractions.Fract.t;
  y: Fractions.Fract.t;
}


type plane_point_t = {
  actual: point_t;
  original: point_t;
}

type line_t = point_t * point_t (* just two points *)
type segment_t = plane_point_t * plane_point_t (* line segment *)
type side_t = Left | Right

(* intersection of line and segment *)
type intersection_t = Miss of side_t | Intersect of segment_t * segment_t * point_t (* left seg, right seg, intersection *)

type facet_t = plane_point_t list

(* ~~~ end types ~~~ *)


(* point to string *)
let p_to_s pt = P.sprintf "%s:%s" (F.to_s pt.x) (F.to_s pt.y)
;;

(* planar point to string *)
let pp_to_s pp = 
  sprintf "%s {%s}" (p_to_s pp.actual) (p_to_s pp.original)
;;

(* planar point list to string *)
let pplist_to_s pp = 
  List.map pp ~f:pp_to_s |> String.concat ~sep:" -> " 
;;

(* line to string *)
let l_to_s l =
  let p1, p2 = l in
  sprintf "LINE[ %s -- %s ]" (p_to_s p1) (p_to_s p2)
;;

let seg_to_s s =
  let s1, s2 = s in
  sprintf "SEG[ %s >> %s ]" (pp_to_s s1) (pp_to_s s2)
;;

let make_point x y = { x = x; y = y }
let make_plane_point n = { actual = n; original = n }


let vector_of_line l =
  let p1, p2 = l in
  let dx = F.sub p2.x p1.x 
  and dy = F.sub p2.y p1.y
  in make_point dx dy
;;


(* assumes that p actually is somewhere on the line given by s *)
let planarize_segment_point s (p:point_t) =

  (* P.printf "planarizing %s for %s\n" (seg_to_s s) (p_to_s p); *)

  let pp1, pp2 = s in

  if F.lt p.x (F.min pp1.actual.x pp2.actual.x) then None
  else if F.gt p.x (F.max pp1.actual.x pp2.actual.x) then None
  else if F.lt p.y (F.min pp1.actual.y pp2.actual.y) then None
  else if F.gt p.y (F.max pp1.actual.y pp2.actual.y) then None
  else 
    let dx = F.sub pp2.actual.x pp1.actual.x in
    let dy = F.sub pp2.actual.y pp1.actual.y in

    let frac_x = if F.is_zero dx then F.zero else F.div (F.sub p.x pp1.actual.x) dx in
    let frac_y = if F.is_zero dy then F.zero else F.div (F.sub p.y pp1.actual.y) dy in

    let odx = F.sub pp2.original.x pp1.original.x in
    let ody = F.sub pp2.original.y pp1.original.y in

    Some {
      actual = p;
      original = {
        x = F.add pp1.original.x (F.mul frac_x odx);
        y = F.add pp1.original.y (F.mul frac_y ody);
      }
    }
;;






let segment_intersect_line s (l:line_t) =
  let pp1, pp2 = s in
  let p1 = pp1.actual in
  let p2 = pp2.actual in
  let p3, p4 = l in

  let denom = F.sub
    (F.mul (F.sub p1.x p2.x) (F.sub p3.y p4.y)) 
    (F.mul (F.sub p1.y p2.y) (F.sub p3.x p4.x)) 
  in

  let cross a b =
    let det = (F.sub (F.mul a.x b.y) (F.mul b.x a.y)) in
    (* printf "DET: %s\n" (F.to_s det); *)
    if F.gte det F.zero then Left else Right
  in

  let line_vector = vector_of_line l in

  let det1 = cross line_vector ( make_point (F.sub p1.x p3.x) (F.sub p1.y p3.y) )
  and det2 = cross line_vector ( make_point (F.sub p2.x p3.x) (F.sub p2.y p3.y) ) in


  if det1 = det2 then Miss det1
  else begin
    let ix = F.sub 
      (F.mul (F.sub (F.mul p1.x p2.y) (F.mul p1.y p2.x)) (F.sub p3.x p4.x))
      (F.mul (F.sub (F.mul p3.x p4.y) (F.mul p3.y p4.x)) (F.sub p1.x p2.x))
    in
    let ix = F.div ix denom
    in

    let iy = F.sub 
      (F.mul (F.sub (F.mul p1.x p2.y) (F.mul p1.y p2.x)) (F.sub p3.y p4.y))
      (F.mul (F.sub (F.mul p3.x p4.y) (F.mul p3.y p4.x)) (F.sub p1.y p2.y))
    in
    let iy = F.div iy denom
    in

    let pt_intersection = make_point ix iy in

    (* P.printf "intersected %s and %s, got %s\n" (seg_to_s s) (l_to_s l) (p_to_s pt_intersection); *)

    (* ix, iy — intersection point *)

    match planarize_segment_point s pt_intersection with
    | None ->
        F.with_human_output (fun () ->
          printf "Segment_intersect_line %s %s\n" (seg_to_s s) (l_to_s l);
          printf "Planarize %s at %s\n" (seg_to_s s) (p_to_s pt_intersection);
        );
        failwith "planarize_segment_point: should not happen"
    | Some plane_pt -> 
        if det1 = Left then
          (Intersect ((pp1, plane_pt), (plane_pt, pp2), pt_intersection))
        else
          (Intersect ((plane_pt, pp2), (pp1, plane_pt), pt_intersection))
  end
;;


let facet_edges facet =
  let out = ref [] in
  let last_pt = ref None in
  List.iter facet ~f:(fun pt ->

    match !last_pt with
    | None ->
        last_pt := Some pt
    | Some lp ->
        out := (lp, pt) :: !out;
        last_pt := Some pt;
  );
  out := ((List.last_exn facet), (List.hd_exn facet)) :: !out;
  List.rev !out
;;

let reflect_point_around_line pt line = 
  let d = vector_of_line line in
  let p0 = fst line in
  let a = 
    (F.div
      (F.sub (F.mul d.x d.x) (F.mul d.y d.y))
      (F.add (F.mul d.x d.x) (F.mul d.y d.y))) in
  let b = 
    (F.div
      (F.mul_int (F.mul d.x d.y) 2)
      (F.add (F.mul d.x d.x) (F.mul d.y d.y))) in
  let xx, yy = (F.sub pt.x p0.x), (F.sub pt.y p0.y) in
  make_point
    (F.add p0.x (F.add (F.mul a xx) (F.mul b yy)))
    (F.add p0.y (F.sub (F.mul b xx) (F.mul a yy)))
;;


let reflect_plane_point_around_line ppt line = 
  { ppt with
    actual = reflect_point_around_line ppt.actual line;
  }
;;


let facet_fold facet line =

  let gather_points pts =

    let rec dedup ptset accum = function 
      | [] -> List.rev accum
      | pt :: rest ->
          let key = pp_to_s pt in
          if String.Set.mem ptset key then dedup ptset accum rest
          else dedup (String.Set.add ptset key) (pt :: accum) rest
    in


    let rec all_pts accum = function
      | segment :: rest -> all_pts ( (snd segment) :: (fst segment) :: accum ) rest
      | [] -> accum
    in

    all_pts [] pts |> dedup String.Set.empty []
  in

  (* target edges *)
  let f_lt = ref [] and f_rt = ref []  in (* new edges *)

  let edges = (facet_edges facet) in

  if debug then List.iter edges ~f:(fun x -> P.eprintf ": %s\n" (seg_to_s x));

  (* ooh the windings will mess up *)
  List.iter edges ~f:(fun edge ->
    begin match segment_intersect_line edge line with
    | Miss dir ->
        if dir = Left
        then f_lt := edge :: !f_lt
        else f_rt := edge :: !f_rt;
    | Intersect (left, right, _)  -> 
        f_lt := left :: !f_lt;
        f_rt := right :: !f_rt;
    end
  );

  let pts_lt = gather_points (List.rev !f_lt) in
  let pts_rt = gather_points (List.rev !f_rt) in

  let pts_rt = List.map pts_rt ~f:(fun pt -> reflect_plane_point_around_line pt line) in

  (* now reflect the correct *)
  if debug then eprintf "Resulting facet points\n";
  if debug then eprintf "LT: %s\n" (pplist_to_s pts_lt);
  if debug then eprintf "RT: %s\n" (pplist_to_s pts_rt);

  let maybe_add_facet pts accu =
    if List.length pts < 3 then accu
    else pts :: accu
  in
  let result = [] |> maybe_add_facet pts_lt |> maybe_add_facet pts_rt in
  if (List.length result) = 1 then [ facet ] else result

;;

let unit_facet () = [
      make_plane_point (make_point F.zero F.zero);
      make_plane_point (make_point F.zero F.one);
      make_plane_point (make_point F.one F.one);
      make_plane_point (make_point F.one F.zero);
    ];
;;

let p_eq p1 p2 =
  (F.eq p1.x p2.x) && (F.eq p1.y p2.y)
;;

let pp_eq p1 p2 =
  (F.eq p1.actual.x p2.actual.x) && (F.eq p1.actual.y p2.actual.y) &&
  (F.eq p1.original.x p2.original.x) && (F.eq p1.original.y p2.original.y)
;;
