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

type winding_t = Cw | Ccw


(* intersection of line and segment *)
type intersection_t = Miss of side_t | Intersect of segment_t * segment_t * point_t (* left seg, right seg, intersection *)

type facet_t = {
  points: plane_point_t list;
  winding: winding_t
}

(* ~~~ end types ~~~ *)

(* val facet_fold: facet_t -> line_t -> facet_t list *)
val facet_fold: facet_t -> line_t -> facet_t list

val p_eq : point_t -> point_t -> bool
val pp_eq : plane_point_t -> plane_point_t -> bool

val p_to_s : point_t -> string
val pp_to_s : plane_point_t -> string
val l_to_s : line_t -> string
val pplist_to_s : plane_point_t list -> string

val make_point : Fractions.Fract.t -> Fractions.Fract.t -> point_t
val make_plane_point : point_t -> plane_point_t
val segment_intersect_line : segment_t -> line_t -> intersection_t
val reflect_plane_point_around_line : plane_point_t -> line_t -> plane_point_t
val reflect_point_around_line : point_t -> line_t -> point_t

val unit_facet : unit -> facet_t
