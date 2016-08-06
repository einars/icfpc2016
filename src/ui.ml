open Core.Std

module F = Fractions

let debug = false

let all_facet_points (facets:Facets.facet_t list) =
  let seen = ref String.Set.empty in
  let pts = ref [] in

  let look_at pp =
    let key = Facets.pp_to_s pp in
    if not (String.Set.mem !seen key) then (
      seen := String.Set.add !seen key;
      pts := pp :: !pts;
    )

  in

  List.iter facets ~f:(fun facet -> List.iter facet ~f:look_at);
  !pts
;;


let choose_random_edge facets =
  let pts = all_facet_points facets in
  let n_pts = List.length pts in

  let choose_point () =
    let idx = Random.int n_pts in
    List.nth_exn pts idx
  in

  let rec choose_point_not pt =
    let px = choose_point () in
    if Facets.pp_eq px pt then choose_point_not pt else px
  in

  let a = choose_point () in
  let b = choose_point_not a in

  a.actual, b.actual
;;


let flatten (facets:Facets.facet_t list list) =
  let out = ref [] in
  List.iter facets ~f:(fun fs -> out := List.append !out fs);
  !out
;;

type map_string_int_t = int String.Map.t

let pp_orig_to_s (pp:Facets.plane_point_t) =
  sprintf "%s,%s" (F.to_s pp.original.x) (F.to_s pp.original.y)


let get_solution facets =

  let repr = ref [] in

  let vertices = all_facet_points facets in

  let vert_map = ref String.Map.empty in
  let idx = ref 0 in

  let pp_key (pp:Facets.plane_point_t) = Facets.p_to_s pp.original in

  List.iter vertices ~f:(fun v -> 
    if not (Map.mem !vert_map (pp_key v)) then begin
      vert_map := Map.add !vert_map ~key:(pp_key v) ~data:!idx;
      idx := !idx +1;
      repr := (sprintf "%s,%s\n" (F.to_s v.original.x) (F.to_s v.original.y)) :: !repr;
      (* repr := (sprintf "%s, %s -> %s, %s\n" (F.to_s v.original.x) (F.to_s v.original.y) (F.to_s v.actual.x) (F.to_s v.actual.y)) :: !repr; *)
    end;
  );
  let n_filtered_pts = List.length !repr in (* hackhackhack, prepend n_points *)
  repr := List.append !repr [sprintf "%d\n" n_filtered_pts];

  repr := (sprintf "%d\n" (List.length facets)) :: !repr;

  List.iter facets ~f:(fun facet ->
    let vertex_indices = List.map facet ~f:(fun pp -> 
      Map.find_exn !vert_map (pp_key pp)
    ) in
    repr := (sprintf "%d " (List.length vertex_indices)) :: !repr;
    let facet = String.concat ~sep:" " (List.map vertex_indices ~f:string_of_int) in
    repr := sprintf "%s\n" facet :: !repr
  );

  List.iter vertices ~f:(fun v -> 
    repr := (sprintf "%s,%s\n" (F.to_s v.actual.x) (F.to_s v.actual.y)) :: !repr;
  );

  String.concat (List.rev !repr);
;;



let choose_very_random_point facets bias =
  let a1,a2 = choose_random_edge facets in
  let dx,dy = (F.sub a2.x a1.x), (F.sub a2.y a1.y) in

  (* choose a point on this line *)
  let rand = Random.int (bias + 1) in

  Facets.make_point 
    (F.add a1.x (F.mul dx (F.make rand bias)))
    (F.add a1.y (F.mul dy (F.make rand bias)))
;;


let choose_line_vx facets =
    let p1,p2 = (choose_very_random_point facets 4), (choose_very_random_point facets 4) in

    let fatness = 4 in
    let ppoint = (1 + Random.int fatness) in (* will choose 1/3, 2/3 or 3/3 *)
    let divisor = F.make ppoint fatness in
    let breakpoint = Facets.make_point (F.add p1.x (F.mul (F.sub p2.x p1.x) divisor)) (F.add p1.y (F.mul (F.sub p2.y p1.y) divisor)) in
    let normal_vect = Facets.make_point (F.sub p2.y p1.y) (F.sub p1.x p2.x) in
    let line = breakpoint, Facets.make_point (F.add breakpoint.x normal_vect.x) (F.add breakpoint.y normal_vect.y) in
    line
;;
let choose_line_45 facets =
  let pt = (choose_very_random_point facets 6) in
  match Random.int 3 with
  | 0 -> pt, Facets.pt_add pt (Facets.make_point F.one F.zero)
  | 1 -> pt, Facets.pt_add pt (Facets.make_point F.zero F.one)
  | 2 -> pt, Facets.pt_add pt (Facets.make_point F.one F.one)
  | 3 -> pt, Facets.pt_add pt (Facets.make_point F.one F.minus_one)
  | 4 -> pt, Facets.pt_add pt (Facets.make_point F.minus_one F.zero)
  | 5 -> pt, Facets.pt_add pt (Facets.make_point F.zero F.minus_one)
  | 6 -> pt, Facets.pt_add pt (Facets.make_point F.minus_one F.minus_one)
  | _ -> pt, Facets.pt_add pt (Facets.make_point F.minus_one F.one)
  
;;


let rec fold_randomly facets = function
  | 0 -> facets
  | n -> 
      (* let line = choose_line_vx facets in *)
      let line = choose_line_45 facets in

      if debug then eprintf "Folding over %s\n%!" (Facets.l_to_s line);
      let new_facets = List.map facets ~f:(fun f -> Facets.facet_fold f line) |> flatten in 
      fold_randomly (if List.length new_facets = List.length facets then facets else new_facets) (n - 1)
;;

let solution_size s =
  String.filter s ~f:(fun c -> c <> ' ' && c <> '\n') |> String.length
;;

let rec fold_until facets min_size max_size =
  let new_facets = fold_randomly facets 1 in
  let solution = get_solution new_facets in
  let sol_len = solution_size solution in
  if debug then eprintf "solution_size: %d\n%!" sol_len; 
  if debug then eprintf "%s\n%!" solution;
  if sol_len >= min_size && sol_len < max_size then new_facets
  else if sol_len > max_size then fold_until facets min_size max_size
  else fold_until new_facets min_size max_size
;;


let run () =

  Tests.run ();

  Random.self_init ();
  let seed = if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else Random.bits () in
  eprintf "Using seed %d\n%!" seed;
  Random.init seed;

  (*
  let fs = [ Facets.unit_facet () ] |> Pythagoras.pythamorph in
  fold_until fs 3000 5000 |> get_solution |> printf "%s";
  *)

  let fs = [ Facets.unit_facet () ] in
  (* fold_until fs 3000 4000 |> Pythagoras.pythamorph |> get_solution |> printf "%s"; *)

  (* fold_until fs 1000 2000 |> Pythagoras.pythamorph |> get_solution |> printf "%s"; *)

  fold_until fs 4000 5000 |> get_solution |> printf "%s";

  (*
  let s1 = fold_until fs 2000 3000 in
  let sol_len_1 = s1 |> get_solution |> solution_size in
  let s2 = s1 |> Pythagoras.pythamorph in
  let sol_len_2 = s2 |> get_solution |> solution_size in
  eprintf "Solution size change: %d -> %d\n" sol_len_1 sol_len_2;
  *)

  ()
