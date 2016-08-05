
module F = Fractions

module P = Printf

let run () =

  let assert_fract expected frac =
    let got = F.to_string frac in
    if got <> expected then
      failwith ( P.sprintf "Expected fraction %s, got %s\n" expected got )
  in

  assert_fract "2" (F.make 4 2) ;
  assert_fract "1/2" (F.make 2 4) ;
  assert_fract "10" (F.of_int 10) ;
  assert_fract "0" (F.of_int 0) ;
  assert_fract "0" (F.mul_int (F.of_int 0) 5) ;
  assert_fract "7/2" (F.mul_int (F.make 2 4) 7) ;
  assert_fract "3/2" (F.add (F.of_int 1) (F.make 2 4)) ;

  P.printf "Tests passed, it's alive!\n";


  ()
