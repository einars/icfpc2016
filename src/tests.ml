open Core.Std

module P = Printf

module F = Fractions
module Fa = Facets


let run () =

  let assert_fract expected frac =
    let got = Fractions.to_s frac in
    if got <> expected then
      failwithf "Expected fraction %s, got %s\n" expected got ()
  in

  F.(
    assert_fract "2" (make 4 2) ;
    assert_fract "1/2" (make 2 4) ;
    assert_fract "10" (of_int 10) ;
    assert_fract "0" (of_int 0) ;
    assert_fract "0" (mul_int (of_int 0) 5) ;
    assert_fract "7/2" (mul_int (make 2 4) 7) ;
    assert_fract "3/2" (add (of_int 1) (make 2 4)) ;
  );

  Facets.(

    let assert_seg_x_line (s1x, s1y) (s2x, s2y) (l1x, l1y) (l2x, l2y) expected =
      let seg1 = make_point (F.of_int s1x) (F.of_int s1y) in
      let seg2 = make_point (F.of_int s2x) (F.of_int s2y) in
      let l1   = make_point (F.of_int l1x) (F.of_int l1y) in
      let l2   = make_point (F.of_int l2x) (F.of_int l2y) in
      let seg = (make_plane_point seg1), (make_plane_point seg2) in
      let l = l1, l2 in
      let got = match segment_intersect_line seg l with
      | Miss Left -> "miss left"
      | Miss Right -> "miss right"
      | Intersect (_, _, pt) -> P.sprintf "intersect %s:%s" (Fractions.to_s pt.x) (Fractions.to_s pt.y)
      in
      if got <> expected then
        failwithf "Expected %s, got %s\n" expected got ()
    in

    let assert_fold_over_line (px, py) (vx, vy) (foldx, foldy) =
      let expected = make_point (F.of_int foldx) (F.of_int foldy) in
      let got = Fa.reflect_point_around_line
        (Fa.make_point (F.of_int px) (F.of_int py))
        ((Fa.make_point F.zero F.zero), (Fa.make_point (F.of_int vx) (F.of_int vy)))
      in
      if Fa.p_to_s expected <> Fa.p_to_s got then
        failwithf "Fold %d:%d over %d:%d expected %s got %s"
          px py vx vy (Fa.p_to_s expected) (Fa.p_to_s got) ()
    in

    assert_seg_x_line (0, -5) (0, 5) (-5, 0) (5, 0) "intersect 0:0";
    assert_seg_x_line (5, -5) (-5, 5) (-5, -5) (5, 5) "intersect 0:0";
    assert_seg_x_line (-1, 1) (-1, 3) (0, 0) (0, 5) "miss left";
    assert_seg_x_line (+1, 1) (+1, 3) (0, 0) (0, 5) "miss right";

    assert_seg_x_line (-1, 1) (-1, 3) (0, 0) (0, -5) "miss right";
    assert_seg_x_line (+1, 1) (+1, 3) (0, 0) (0, -5) "miss left";

    assert_seg_x_line (2, 3) (4, 1) (-6, -4) (+6, +4) "intersect 3:2";
    assert_seg_x_line (2, 3) (4, 1) (+6, +4) (-6, -4) "intersect 3:2";

    assert_fold_over_line (1, 1) (0, 5) (-1, +1);
    assert_fold_over_line (1, 1) (5, 0) (+1, -1);

    assert_fold_over_line (3, 0) (1, 1) (+0, +3);
    assert_fold_over_line (0, 3) (1, 1) (+3, +0);

    let unit_facet_l = {
      points = [
        make_plane_point (make_point F.zero F.zero);
        make_plane_point (make_point F.zero F.one);
        make_plane_point (make_point F.one F.one);
        make_plane_point (make_point F.one F.zero);
      ];
      winding = Cw; (* front plane *)
    } in

    let unit_facet_r = {
      points = List.rev unit_facet_l.points;
      winding = Ccw; (* back plane *)
    } in

    let fold_line = (make_point F.zero (F.make 1 2)), (make_point F.one F.one) in
    facet_fold unit_facet_l fold_line |> ignore;
    facet_fold unit_facet_r fold_line |> ignore;

    (* nekrustojas, nefoldojas *)
    facet_fold unit_facet_l ((make_point (F.make 2 1) F.zero), (make_point (F.make 2 1) F.one)) |> ignore;



  );

  P.printf "Tests passed, it's alive!\n";


  ()
