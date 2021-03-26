(** Tests for modules:

    - Math
    - Vector
    - Matrix *)

open OUnit2
open Reals
open Vector

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

let vector_test_add_elt
    (name : string)
    (v : t)
    (e : elt)
    (expected_output : t) : test =
  name >:: fun _ ->
  assert_equal expected_output (add_elt v e) ~cmp:vector_equality
    ~printer:string_of_vector

let vector_test_sum
    (name : string)
    (v1 : t)
    (v2 : t)
    (expected_output : t) : test =
  name >:: fun _ ->
  assert_equal expected_output (sum v1 v2) ~cmp:vector_equality
    ~printer:string_of_vector

let vector_test_dot
    (name : string)
    (v1 : t)
    (v2 : t)
    (expected_output : elt) : test =
  name >:: fun _ ->
  assert_equal expected_output (dot v1 v2) ~printer:string_of_real

let vector_test_scalar_mult
    (name : string)
    (v : t)
    (e : elt)
    (expected_output : t) : test =
  name >:: fun _ ->
  assert_equal expected_output (scalar_mult e v) ~cmp:vector_equality
    ~printer:string_of_vector

let vector_test_subtract
    (name : string)
    (v1 : t)
    (v2 : t)
    (expected_output : t) : test =
  name >:: fun _ ->
  assert_equal expected_output (subtract v1 v2) ~cmp:vector_equality
    ~printer:string_of_vector

let vector_test_cross
    (name : string)
    (v1 : t)
    (v2 : t)
    (expected_output : t) : test =
  name >:: fun _ ->
  assert_equal expected_output (cross v1 v2) ~cmp:vector_equality
    ~printer:string_of_vector

let empty_vec = of_reals_list []

let zero_vec = of_reals_list [ Reals.Zero ]

let one_vec = of_reals_list [ Reals.Float 1. ]

let vector_tests =
  let open Reals in
  [
    vector_test_add_elt "adding element to empty vector list" empty_vec
      Zero zero_vec;
    vector_test_add_elt "adding element to non-empty vector list"
      zero_vec (Float 1.5)
      (of_reals_list [ Zero; Float 1.5 ]);
    vector_test_sum "adding two empty vector lists" empty_vec empty_vec
      empty_vec;
    vector_test_sum "adding two one-element vector lists" zero_vec
      (of_reals_list [ Float 1. ])
      (of_reals_list [ Float 1. ]);
    vector_test_sum "adding two two-element vector lists"
      (of_reals_list [ Zero; Rational (1, 5) ])
      (of_reals_list [ Float 1.; Zero ])
      (of_reals_list [ Float 1.; Rational (1, 5) ]);
    ( "adding two different dimension vectors" >:: fun _ ->
      assert_raises Vector.Dimension_Mismatch (fun () ->
          Vector.sum empty_vec zero_vec) );
    vector_test_dot "dotting two one-element vector lists" zero_vec
      (of_reals_list [ Float 1. ])
      Reals.Zero;
    vector_test_dot "dotting two two-element vector lists"
      (of_reals_list [ Float 1.; Rational (2, 4) ])
      (of_reals_list [ Float 6.; Float (-7.4) ])
      (Reals.Float 2.3);
    ( "dotting two different dimension vectors" >:: fun _ ->
      assert_raises Vector.Dimension_Mismatch (fun () ->
          Vector.dot empty_vec zero_vec) );
    vector_test_scalar_mult "scalar mult an empty list" empty_vec
      (Rational (2, 5))
      empty_vec;
    vector_test_scalar_mult "scalar mult an one element list" one_vec
      (Rational (2, 5))
      (of_reals_list [ Rational (2, 5) ]);
    vector_test_scalar_mult "scalar mult an one element list"
      (of_reals_list [ Rational (2, 5); Zero ])
      (Rational (4, 5))
      (of_reals_list [ Rational (8, 25); Zero ]);
    vector_test_subtract "subtracting two empty vector lists" empty_vec
      empty_vec empty_vec;
    vector_test_subtract "subtracting two one-element vector lists"
      zero_vec
      (of_reals_list [ Float 1. ])
      (of_reals_list [ Float (-1.) ]);
    vector_test_subtract "subtract two two-element vector lists"
      (of_reals_list [ Zero; Rational (1, 5) ])
      (of_reals_list [ Float 1.; Zero ])
      (of_reals_list [ Float (-1.); Rational (1, 5) ]);
    ( "subtract two different dimension vectors" >:: fun _ ->
      assert_raises Vector.Dimension_Mismatch (fun () ->
          Vector.subtract empty_vec zero_vec) );
    vector_test_cross "cross two 3 element vector lists "
      (of_reals_list [ Float 1.; Float 2.; Float 3. ])
      (of_reals_list [ Float 1.; Float 5.; Float 7. ])
      (of_reals_list [ Float (-1.); Float (-4.); Float 3. ]);
    ( "cross vectors that are not 3 dimension" >:: fun _ ->
      assert_raises Vector.Dimension_Mismatch (fun () ->
          Vector.cross empty_vec zero_vec) );
  ]

let test_list = List.flatten [ reals_tests; vector_tests ]
