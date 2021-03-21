(** Tests for modules:

    - Math
    - Vector
    - Matrix *)

open OUnit2
open Reals

let real_test_binary
    ?(printer : 'a -> string = fun _ -> "useless print")
    ?(cmp : 'a -> 'a -> bool = fun a b -> a = b)
    op_name
    op
    arg1
    arg2
    expected =
  string_of_real arg1 ^ " =: " ^ string_of_real arg2 ^ " is "
  ^ printer expected
  >:: fun _ -> assert_equal expected (op arg1 arg2) ~cmp ~printer

let real_test_equal =
  real_test_binary ~printer:string_of_bool " =: " ( =: )

let real_test_add =
  real_test_binary ~printer:string_of_real ~cmp:( =: ) " +: " ( +: )

let real_test_subtract =
  real_test_binary ~printer:string_of_real ~cmp:( =: ) " -: " ( -: )

let real_test_multiply =
  real_test_binary ~printer:string_of_real ~cmp:( =: ) " *: " ( *: )

let real_test_divide =
  real_test_binary ~printer:string_of_real ~cmp:( =: ) " /: " ( /: )

let real_test_unary name op arg expected =
  name >:: fun _ -> assert_equal expected (op arg)

let real_of_int i = Reals.Rational (i, 1)

let reals_tests =
  let open Reals in
  [
    real_test_equal Zero Zero true;
    real_test_equal (Rational (1, 2)) (Rational (1, 2)) true;
    real_test_equal (Rational (1, 2)) (Rational (6, 12)) true;
    real_test_equal (Rational (1, 2)) (Rational (5, 12)) false;
    real_test_equal (Rational (1, 2)) (Float 0.5) true;
    real_test_equal (Float 0.5) (Float 0.5) true;
    real_test_equal (Float 0.5) (Float 0.4) false;
    real_test_add Zero Zero Zero;
    real_test_add (Rational (1, 2)) Zero (Rational (1, 2));
    real_test_add Zero (Float 1.) (Float 1.);
    real_test_subtract Zero (Rational (1, 2)) (Rational (-1, 2));
    real_test_subtract (Rational (1, 2)) Zero (Rational (1, 2));
    real_test_subtract (Rational (1, 2)) (Float 0.3) (Float 0.2);
    real_test_subtract (Float 0.5) (Float 0.2) (Float 0.3);
    real_test_subtract (Float 0.2) (Float 0.5) (Float ~-.0.3);
    real_test_multiply Zero (Float 1.2) Zero;
    real_test_multiply (Rational (1, 4)) Zero Zero;
    real_test_multiply
      (Rational (1, 4))
      (Rational (4, 1))
      (Rational (1, 1));
    real_test_multiply (Float 1.8) (Rational (8, 1)) (Float 14.4);
    real_test_divide Zero (Float 1.666) Zero;
    ( "1.5 /: Zero raises Division_by_zero" >:: fun _ ->
      assert_raises Division_by_zero (fun () -> Float 1.5 /: Zero) );
    real_test_divide
      (Rational (4, 1))
      (Rational (3, 1))
      (Rational (4, 3));
  ]

let test_list = List.flatten [ reals_tests ]
