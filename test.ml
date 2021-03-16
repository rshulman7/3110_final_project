open OUnit2
open Reals

let real_test name = name >:: fun _ -> assert_equal false false

let real_of_int i = Reals.Rational (i, 1)

let reals_tests =
  let open Reals in
  [
    ( "1 + 2 as rationals is 3 as rational" >:: fun _ ->
      assert_equal (real_of_int 3) (real_of_int 1 +: real_of_int 2) );
  ]

let suite = "test suite for A2" >::: List.flatten [ reals_tests ]

let _ = run_test_tt_main suite
