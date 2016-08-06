open Core.Std

module F = Fractions

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

  List.iter facets ~f:(fun facet -> List.iter facet.points ~f:look_at);
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
    if px = pt then choose_point_not pt else px
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

let print_solution facets =

  let vertices = all_facet_points facets in
  printf "%d\n" (List.length vertices);

  let vert_map = ref String.Map.empty in
  let idx = ref 0 in

  List.iter vertices ~f:(fun v -> 
    vert_map := Map.add !vert_map ~key:(Facets.pp_to_s v) ~data:!idx;
    idx := !idx +1;
    printf "%s,%s\n" (F.to_s v.original.x) (F.to_s v.original.y)
  );

  printf "%d\n" (List.length facets);

  List.iter facets ~f:(fun f ->
    let vertex_indices = List.map f.points ~f:(fun pp -> 
      Map.find_exn !vert_map (Facets.pp_to_s pp)
    ) in
    printf "%d " (List.length vertex_indices);
    String.concat ~sep:" " (List.map vertex_indices ~f:string_of_int) |> printf "%s\n"
  );

  List.iter vertices ~f:(fun v -> 
    printf "%s,%s\n" (F.to_s v.actual.x) (F.to_s v.actual.y)
  );

  (* vert_map now has mapping string -> int, where key is the original point representation *)
;;



let choose_very_random_point facets =
  let a1,a2 = choose_random_edge facets in
  let dx,dy = (F.sub a2.x a1.x), (F.sub a2.y a1.y) in

  (* choose a point on this line *)
  let rand = Random.int 3 in

  Facets.make_point 
    (F.add a1.x (F.mul dx (F.make rand 2)))
    (F.add a1.y (F.mul dy (F.make rand 2)))
;;


let rec fold_randomly facets = function
  | 0 -> facets
  | n -> 
      let line = (choose_very_random_point facets), (choose_very_random_point facets) in

      printf "Folding over %s\n" (Facets.l_to_s line);
      let new_facets = List.map facets ~f:(fun f -> Facets.facet_fold f line) |> flatten in 
      fold_randomly new_facets (n - 1)
;;

let run () =

  Tests.run ();

  Random.self_init ();

  let fs = [ Facets.unit_facet () ] in

  fold_randomly fs 5 |> print_solution;

  ()
