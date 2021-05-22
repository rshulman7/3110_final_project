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
  string_of_real arg1 ^ op_name ^ string_of_real arg2 ^ " is "
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
      assert_raises Vector.Dimension_mismatch (fun () ->
          Vector.sum empty_vec zero_vec) );
    vector_test_dot "dotting two one-element vector lists" zero_vec
      (of_reals_list [ Float 1. ])
      Zero;
    vector_test_dot "dotting two two-element vector lists"
      (of_reals_list [ Float 1.; Rational (2, 4) ])
      (of_reals_list [ Float 6.; Float (-7.4) ])
      (Float 2.3);
    ( "dotting two different dimension vectors" >:: fun _ ->
      assert_raises Vector.Dimension_mismatch (fun () ->
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
      assert_raises Vector.Dimension_mismatch (fun () ->
          Vector.subtract empty_vec zero_vec) );
    vector_test_cross "cross two 3 element vector lists "
      (of_reals_list [ Float 1.; Float 2.; Float 3. ])
      (of_reals_list [ Float 1.; Float 5.; Float 7. ])
      (of_reals_list [ Float (-1.); Float (-4.); Float 3. ]);
    ( "cross vectors that are not 3 dimension" >:: fun _ ->
      assert_raises Vector.Dimension_mismatch (fun () ->
          Vector.cross empty_vec zero_vec) );
    ( "2-norm of [2,1] is sqrt(5)" >:: fun _ ->
      assert_equal (sqrt (Float 5.))
        ([ Float 2.; Float 1. ] |> of_reals_list |> norm) );
    ( "2-norm of [2,11] is sqrt(125)" >:: fun _ ->
      assert_equal (sqrt (Float 125.))
        ([ Float 2.; Float 11. ] |> of_reals_list |> norm) );
  ]

open Matrix

let two_by_two_zero =
  of_real_list_list [ [ Zero; Zero ]; [ Zero; Zero ] ]

let two_by_one = of_real_list_list [ [ Float 1. ]; [ Float 1. ] ]

(* [ 2, 1; 
 * 11, 5 ] *)
let two_by_two =
  of_real_list_list [ [ Float 2.; Float 1. ]; [ Float 11.; Float 5. ] ]

let three_by_three =
  of_real_list_list
    [
      [ Float 2.; Float 2.; Zero ];
      [ Rational (2, 5); Float (-1.); Float 1. ];
      [ Zero; Float (-10.); Zero ];
    ]

let three_by_three_mat =
  of_real_list_list
    [
      [ Float 3.; Float 2.; Float 1. ];
      [ Float 2.; Float 2.; Float 1. ];
      [ Float 1.; Float 1.; Float 1. ];
    ]

(* [   1,   3,   0,  4 ;
 *    -1,  -1, 4/3, 3/2;
 *     0,-7/3,   6, 5/6;
 *     1,-1/5,   0,   2;  ]*)
let four_by_four_mat =
  of_real_list_list
    [
      [ Rational (1, 1); Rational (3, 1); Zero; Rational (4, 1) ];
      [
        Rational (-1, 1);
        Rational (-1, 1);
        Rational (4, 3);
        Rational (3, 2);
      ];
      [ Zero; Rational (-7, 3); Rational (6, 1); Rational (5, 6) ];
      [ Rational (1, 1); Rational (-1, 5); Zero; Rational (2, 1) ];
    ]

let id3 =
  of_real_list_list
    [
      [ Rational (1, 1); Zero; Zero ];
      [ Zero; Rational (1, 1); Zero ];
      [ Zero; Zero; Rational (1, 1) ];
    ]

let id2 =
  of_real_list_list
    [ [ Rational (1, 1); Zero ]; [ Zero; Rational (1, 1) ] ]

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
    ( "remove 1st column from two_by_two" >:: fun _ ->
      assert_equal
        (of_real_list_list [ [ Float 1. ]; [ Float 5. ] ])
        (rem_col 0 two_by_two) ~printer:to_string );
    ( "remove 2nd column from two_by_two" >:: fun _ ->
      assert_equal
        (of_real_list_list [ [ Float 2. ]; [ Float 11. ] ])
        (rem_col 1 two_by_two) ~printer:to_string );
    ( "remove 2nd row from two_by_two" >:: fun _ ->
      assert_equal
        (of_real_list_list [ [ Float 2.; Float 1. ] ])
        (rem_row 1 two_by_two) ~printer:to_string );
    ( "remove 2nd row from id3" >:: fun _ ->
      assert_equal
        (of_real_list_list
           [
             [ Float 1.; Float 0.; Float 0. ];
             [ Float 0.; Float 0.; Float 1. ];
           ])
        (rem_row 1 id3) ~printer:to_string ~cmp:matrix_equality );
    ( "remove 2nd col from id3" >:: fun _ ->
      assert_equal
        (of_real_list_list
           [
             [ Float 1.; Float 0. ];
             [ Float 0.; Float 0. ];
             [ Float 0.; Float 1. ];
           ])
        (rem_col 1 id3) ~printer:to_string ~cmp:matrix_equality );
  ]

open Linearalgops

let one_by_one = of_real_list_list [ [ Float 2. ] ]

let one_by_one_vec = of_reals_list [ Reals.Float 1. ]

let one_by_one_sol =
  of_real_list_list [ [ Reals.Float 1.; Reals.Float 0.5 ] ]

let two_by_one_vec = of_reals_list [ Reals.Float 1.; Reals.Float 1. ]

let two_by_two_sol =
  of_real_list_list
    [
      [ Reals.Float 1.; Zero; Reals.Float (-4.) ];
      [ Zero; Reals.Float 1.; Reals.Float 9. ];
    ]

let three_by_one_vec =
  of_reals_list [ Reals.Float 1.; Reals.Float 1.; Reals.Float 1. ]

let m4eig = of_real_list_list [ [ Float 2.; Zero ]; [ Zero; Float 1. ] ]

let three_by_three_sol =
  of_real_list_list
    [
      [ Reals.Float 1.; Zero; Zero; Zero ];
      [ Zero; Reals.Float 1.; Zero; Zero ];
      [ Zero; Zero; Reals.Float 1.; Reals.Float 1. ];
    ]

let ops_test_rref
    (name : string)
    (m : Matrix.t)
    (v : Vector.t)
    (expected_output : Matrix.t) : test =
  name >:: fun _ ->
  assert_equal expected_output (rref m v) ~cmp:matrix_equality
    ~printer:to_string

let lst_print lst =
  let rec lst_print_help lst =
    match lst with
    | [] -> "empty"
    | [ h ] -> Reals.string_of_real h ^ "]"
    | h :: t -> Reals.string_of_real h ^ ", " ^ lst_print_help t
  in
  "[" ^ lst_print_help lst

let tol = 1e-4

let close_enough_comparison a b = Reals.(abs (a -: b) <: Float tol)

let list_comparison = List.for_all2 close_enough_comparison

let op_tests =
  [
    ops_test_rref "first test" one_by_one one_by_one_vec one_by_one_sol;
    ops_test_rref "second test" two_by_two two_by_one_vec two_by_two_sol;
    ops_test_rref "third test" three_by_three_mat three_by_one_vec
      three_by_three_sol;
    ( "determinant of one_by_one is 2" >:: fun _ ->
      assert_equal (Float 2.) (det one_by_one) );
    ( "determinant of 2x2 identity is\n      1" >:: fun _ ->
      assert_equal (Rational (1, 1)) (det id2) );
    ( "determinant of 3x3 identity is 1" >:: fun _ ->
      assert_equal (Rational (1, 1)) (det id3) );
    ( "determinant of three_by_three_mat is 1." >:: fun _ ->
      assert_equal (Float 1.)
        (det three_by_three_mat)
        ~printer:string_of_real );
    ( "determinant of four_by_four is 3232/45" >:: fun _ ->
      assert_equal
        (Rational (3232, 45))
        (det four_by_four_mat) ~printer:string_of_real ~cmp:( =: ) );
    ( "eigenvalues of [2, 1; 0, 1] are [2, 1]" >:: fun _ ->
      let eigs, vecs = m4eig |> eig in
      assert_equal [ Float 2.; Float 1. ] eigs );
    ( "eigenvalues of [2, 1; 11, 5] are about [7.14, -0.14]" >:: fun _ ->
      assert_equal
        [ Float 7.14; Float (-0.14) ]
        (two_by_two |> eig |> fst)
        ~cmp:list_comparison ~printer:lst_print );
    ( "eigenvalues of three_by_three_mat are about [5.0489; 0.6431; \
       0.30797]"
    >:: fun _ ->
      assert_equal
        [ Float 5.0489; Float 0.6431; Float 0.30797 ]
        (three_by_three_mat |> eig |> fst)
        ~cmp:list_comparison ~printer:lst_print );
    ( "can't compute complex eigenvalues, so should throw a timeout \
       error"
    >:: fun _ ->
      assert_raises (Timeout "QR algorithm did not converge") (fun () ->
          eig
            (of_real_list_list
               [ [ Float 0.; Float 1. ]; [ Float 1.; Float 0. ] ])) );
    (* ( "eig quality" >:: fun _ -> assert_equal () (check_quality_eig
       two_by_two) ); ( "eig quality" >:: fun _ -> assert_equal ()
       (check_quality_eig three_by_three_mat) ); *)
    ( "inverse of id2 is id2" >:: fun _ ->
      assert_equal id2 (Matrix.inverse id2) ~printer:Matrix.to_string
        ~cmp:Matrix.matrix_equality );
    ( "inverse of id3 is id3" >:: fun _ ->
      assert_equal id3 (Matrix.inverse id3) ~printer:Matrix.to_string
        ~cmp:Matrix.matrix_equality );
    ( "inverse of two_by_two" >:: fun _ ->
      assert_equal
        Matrix.(
          of_real_list_list
            [ [ Float ~-.5.; Float 1. ]; [ Float 11.; Float ~-.2. ] ])
        (Matrix.inverse two_by_two)
        ~printer:Matrix.to_string ~cmp:Matrix.matrix_equality );
  ]

let mat_ode_test =
  of_real_list_list
    [ [ Float 2.; Float 1.; Zero ]; [ Float 1.; Float 1.; Zero ] ]

let mat_ode_test_2 =
  of_real_list_list
    [ [ Float 2.; Zero; Float 1. ]; [ Zero; Float 1.; Float 1. ] ]

let vec_init = of_reals_list [ Float 2.; Float 1. ]

let vec_init_2 = of_reals_list [ Zero; Zero ]

open Ode_solver

let ode_tests =
  [
    ( "x' = 2x+ y, y' = y with initial condition [v1; v2] has solution \
       [v1; v2] at time 0"
    >:: fun _ ->
      assert_equal
        (to_reals_list vec_init)
        (exact_linear_solver mat_ode_test vec_init Zero
        |> Vector.to_reals_list)
        ~cmp:list_comparison ~printer:lst_print );
    (* ( "eig quality" >:: fun _ -> assert_equal () (check_quality_eig
       mat_ode_test) ); *)
    ( "x' = 2x + y, y' = x + y with initial condition [2; 1] has \
       solution [26.1249; 15.8002] at time 1"
    >:: fun _ ->
      assert_equal
        [ Float 26.1249; Float 15.8002 ]
        (exact_linear_solver mat_ode_test vec_init (Float 1.)
        |> Vector.to_reals_list)
        ~cmp:list_comparison ~printer:lst_print );
    ( "x' = 2x + 1, y' = y + 1 with initial condition [0; 0] has \
       solution [ $ (e^{2t} - 1) / 2 $; $ e^t - 1 $] at time t"
    >:: fun _ ->
      assert_equal
        [ Float 3.1945; Float 1.71828 ]
        (exact_linear_solver mat_ode_test_2 vec_init_2 (Float 1.)
        |> Vector.to_reals_list)
        ~cmp:list_comparison ~printer:lst_print );
    ( "x' = x with initial condition x0 has solution x0e^t " >:: fun _ ->
      assert_equal [ Float 2.7183 ]
        (exact_linear_solver
           (of_real_list_list [ [ Float 1.; Zero ] ])
           (of_reals_list [ Float 1. ])
           (Float 1.)
        |> Vector.to_reals_list)
        ~cmp:list_comparison ~printer:lst_print );
  ]

let euler_test
    (name : string)
    (plot : bool)
    (m : Euler.t)
    (init_cond : Euler.v)
    (end_time : Euler.elt)
    (step_size : Euler.elt)
    (expected_output : Euler.v) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Euler.euler plot m init_cond end_time step_size)
    ~cmp:vector_equality ~printer:Vector.string_of_vector

let runge_test
    (name : string)
    (plot : bool)
    (m : Runge.t)
    (init_cond : Runge.v)
    (end_time : Runge.elt)
    (step_size : Runge.elt)
    (expected_output : Runge.v) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Runge.rk plot m init_cond end_time step_size)
    ~cmp:vector_equality ~printer:Vector.string_of_vector

let m1 = Matrix.of_real_list_list [ [ Reals.Float 1.; Reals.Zero ] ]

let v1 = Vector.of_reals_list [ Reals.Float 1. ]

let m2 =
  Matrix.of_real_list_list
    [
      [ Reals.Float 1.; Reals.Zero; Reals.Zero ];
      [ Reals.Zero; Reals.Float 1.; Reals.Zero ];
    ]

let m3 =
  Matrix.of_real_list_list
    [
      [ Reals.Zero; Reals.Float (-1.); Reals.Zero ];
      [ Reals.Float 1.; Reals.Zero; Reals.Zero ];
    ]

let v2 = Vector.of_reals_list [ Reals.Float 1.; Reals.Float 1. ]

let v3 = Vector.of_reals_list [ Reals.Float 1.; Reals.Zero ]

let end_time1 = Reals.Float 1.

let end_time3 = Reals.Float 3.1

let step_size1 = Reals.Float 0.5

let step_size3 = Reals.Float 0.1

let euler_sol1 =
  Vector.of_reals_list [ Reals.Float 1.; Reals.Float 2.25 ]

let euler_sol2 =
  Vector.of_reals_list
    [ Reals.Float 1.; Reals.Float 2.25; Reals.Float 2.25 ]

let euler_sol3 =
  Vector.of_reals_list
    [
      Reals.Float 3.1;
      Reals.Float (-1.16519046795);
      Reals.Float 0.0604861792551;
    ]

let rk_sol1 =
  Vector.of_reals_list [ Reals.Float 1.; Reals.Float 2.71734619141 ]

let rk_sol2 =
  Vector.of_reals_list
    [
      Reals.Float 1.;
      Reals.Float 2.71734619141;
      Reals.Float 2.71734619141;
    ]

let rk_sol3 =
  Vector.of_reals_list
    [
      Reals.Float 3.1;
      Reals.Float (-1.16541551123);
      Reals.Float 0.0620316501273;
    ]

let finite_difference_tests =
  [
    euler_test "euler first test" false m1 v1 end_time1 step_size1
      euler_sol1;
    euler_test "euler second test" false m2 v2 end_time1 step_size1
      euler_sol2;
    euler_test "euler third test" false m3 v3 end_time3 step_size3
      euler_sol3;
    runge_test "runge first test" false m1 v1 end_time1 step_size1
      rk_sol1;
    runge_test "runge second test" false m2 v2 end_time1 step_size1
      rk_sol2;
    runge_test "runge third test" false m3 v3 end_time3 step_size3
      rk_sol3;
  ]

let test_list =
  List.flatten
    [
      reals_tests;
      vector_tests;
      matrix_tests;
      op_tests;
      ode_tests;
      finite_difference_tests;
    ]
