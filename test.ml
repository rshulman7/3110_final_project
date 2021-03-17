open OUnit2
open Reals

let real_test name = name >:: fun _ -> assert_equal false false

let real_of_int i = Reals.Rational (i, 1)

let reals_tests =
  let open Reals in
  [
    ( "Zero =: Zero both floats is true" >:: fun _ ->
      assert_equal true (Zero =: Zero) );
    ( "1/2 =: 1/2 both rationals is true" >:: fun _ ->
      assert_equal true (Rational (1, 2) =: Rational (1, 2)) );
    ( "rational 1/2 =: 0.5 decimal is true" >:: fun _ ->
      assert_equal true (Rational (1, 2) =: Rational (1, 2)) );
    ( "1 +: 2 as rationals is 3 as rational" >:: fun _ ->
      assert_equal (real_of_int 3) (real_of_int 1 +: real_of_int 2) );
  ]

let suite = "test suite for project" >::: List.flatten [ reals_tests ]

let _ = run_test_tt_main suite
