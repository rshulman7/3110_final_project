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

open Vector

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
  let open Vector in
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

open Matrix

let two_by_two_zero =
  of_real_list_list [ [ Zero; Zero ]; [ Zero; Zero ] ]

let two_by_one = of_real_list_list [ [ Float 1. ]; [ Float 1. ] ]

let two_by_two =
  of_real_list_list [ [ Float 2.; Float 1. ]; [ Float 11.; Float 5. ] ]

let three_by_three =
  of_real_list_list
    [
      [ Float 2.; Float 2.; Zero ];
      [ Rational (2, 5); Float (-1.); Float 1. ];
      [ Zero; Float (-10.); Zero ];
    ]

let int_tuple_printer (tup : int * int) : string =
  "(" ^ string_of_int (fst tup) ^ ", " ^ string_of_int (snd tup) ^ ")"

let int_tuple_equality (tup1 : int * int) (tup2 : int * int) : bool =
  fst tup1 = fst tup2 && snd tup1 = snd tup2

let matrix_test_size
    (name : string)
    (m : t)
    (expected_output : int * int) : test =
  name >:: fun _ ->
  assert_equal expected_output (size m) ~cmp:int_tuple_equality
    ~printer:int_tuple_printer

let matrix_test_sum
    (name : string)
    (m1 : t)
    (m2 : t)
    (expected_output : t) : test =
  name >:: fun _ ->
  assert_equal expected_output (sum m1 m2) ~cmp:matrix_equality
    ~printer:to_string

let matrix_test_scalar_mult
    (name : string)
    (m : t)
    (e : elt)
    (expected_output : t) : test =
  name >:: fun _ ->
  assert_equal expected_output (scalar_mult e m) ~cmp:matrix_equality
    ~printer:to_string

let matrix_test_multiply
    (name : string)
    (m1 : t)
    (m2 : t)
    (expected_output : t) : test =
  name >:: fun _ ->
  assert_equal expected_output (multiply m1 m2) ~cmp:matrix_equality
    ~printer:to_string

let matrix_tests =
  [
    ( "creating matrix with rows are not all the same length"
    >:: fun _ ->
      assert_raises
        (Matrix.Invalid_matrix "Rows are not all the same length!")
        (fun () -> of_real_list_list [ [ Zero ]; [] ]) );
    matrix_test_size "testing two by two size" two_by_two (2, 2);
    matrix_test_size "testing two by one size" two_by_one (2, 1);
    matrix_test_sum "testing two by two zeros sum" two_by_two_zero
      two_by_two_zero two_by_two_zero;
    matrix_test_sum "testing two by two zeros sum" two_by_two_zero
      two_by_two two_by_two;
    matrix_test_scalar_mult "testing two by two scalar by 2" two_by_two
      (Float 2.)
      (of_real_list_list
         [ [ Float 4.; Float 2. ]; [ Float 22.; Float 10. ] ]);
    matrix_test_multiply "multiply two by two with two by one"
      two_by_two two_by_one
      (of_real_list_list [ [ Float 3. ]; [ Float 16. ] ]);
    matrix_test_multiply "multiply three by three with three by three"
      three_by_three three_by_three
      (of_real_list_list
         [
           [ Float 4.8; Float 2.; Float 2. ];
           [ Rational (2, 5); Float (-8.2); Float (-1.) ];
           [ Float (-4.); Float 10.; Float (-10.) ];
         ]);
  ]

let test_list = List.flatten [ reals_tests; vector_tests; matrix_tests ]
