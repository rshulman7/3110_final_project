open OUnit2
open Io
open Reals

(* checks equality of 2 matrices of Reals *)
let matrix_eq mat_a mat_b =
  let lst_a = List.flatten mat_a in
  let lst_b = List.flatten mat_b in
  if List.length lst_a <> List.length lst_b then false
  else
    let rec real_eq lst_a lst_b eq_val =
      if eq_val then
        match lst_a with
        | h1 :: t1 -> (
            match lst_b with
            | h2 :: t2 -> real_eq t1 t2 (h1 =: h2 && eq_val)
            | [] -> eq_val )
        | [] -> eq_val
      else eq_val
    in
    real_eq lst_a lst_b true

let pp_elt = Reals.string_of_real

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let multi_printer lst_of_lsts =
  let rec print_helper = function
    | [] -> ""
    | h :: t ->
        pp_list pp_elt h
        ^ (if t = [] then "" else "; ")
        ^ print_helper t
  in
  "[" ^ print_helper lst_of_lsts ^ "]"

(* for string list list *)
let multi_printer2 lst_of_lsts =
  let rec print_helper = function
    | [] -> ""
    | h :: t ->
        pp_list (fun x -> x) h
        ^ (if t = [] then "" else ";\n ")
        ^ print_helper t
  in
  "[" ^ print_helper lst_of_lsts ^ "]"

(* helper function for io_tests which tests parse_matrix. Tests valid
   input (i.e. no exn raised) if exn_bin = 0; otherwise tests if exn
   raised *)
let pm_test name exp_matrix input_str exn_bin =
  if exn_bin = 0 then
    "[parse_matrix] test: " ^ name >:: fun _ ->
    assert_equal ~cmp:matrix_eq ~printer:multi_printer exp_matrix
      (parse_matrix input_str)
  else
    "[parse_matrix] exn test: " ^ name >:: fun _ ->
    assert_equal "exn thrown"
      ( match parse_matrix input_str with
      | exception Io.Invalid_input -> "exn thrown"
      | _ -> "" )

let prime_tester name expected_rows expected_primes input =
  "[parse_matrix] test: " ^ name >:: fun _ ->
  make_rows input;
  assert_equal ~printer:multi_printer2 expected_rows
    input.processed_rows;
  assert_equal expected_primes input.primes

(** helper function to simplify testing [fold_tree] *)
let ft_pm_test name exp_matrix input_tree =
  "[fold_tree] test: " ^ name >:: fun _ ->
  assert_equal ~cmp:matrix_eq ~printer:multi_printer exp_matrix
    (fold_tree input_tree)

(** making a tree like tree1 below but using [parse_matrix_eq] *)
let tree1_maker () =
  let mat_var1 : Io.matrix_var =
    {
      name = "a";
      matrix = [ [ Float 1.4; Rational (4, 3) ]; [ Zero; Float 1.567 ] ];
    }
  in
  let mat_var2 : Io.matrix_var =
    {
      name = "b";
      matrix =
        [
          [ Rational (1, 1); Zero ];
          [ Rational (7, 3); Rational (2, 1) ];
        ];
    }
  in
  let tree1_matrix_eq : matrix_eq =
    { matrix_lst = [ mat_var1; mat_var2 ]; equ = "a+b" }
  in
  parse_matrix_eq tree1_matrix_eq

(** making a tree like tree2 below but using [parse_matrix_eq] *)
let tree2_maker () =
  let mat_var1 : Io.matrix_var =
    {
      name = "a";
      matrix = [ [ Float 1.4; Rational (4, 3) ]; [ Zero; Float 1.567 ] ];
    }
  in
  let mat_var2 : Io.matrix_var =
    {
      name = "b";
      matrix =
        [
          [ Rational (1, 1); Zero ];
          [ Rational (6, 3); Rational (2, 1) ];
        ];
    }
  in
  let mat_var3 : Io.matrix_var =
    {
      name = "c";
      matrix =
        [ [ Rational (-2, 1); Rational (4, 3) ]; [ Float 1.5; Zero ] ];
    }
  in
  let tree2_matrix_eq : matrix_eq =
    { matrix_lst = [ mat_var1; mat_var2; mat_var3 ]; equ = "a+b+c" }
  in
  parse_matrix_eq tree2_matrix_eq

(** making a tree like tree3 below but using [parse_matrix_eq] *)
let tree3_maker () =
  let mat_var1 : Io.matrix_var =
    {
      name = "a";
      matrix = [ [ Float 1.4; Rational (4, 3) ]; [ Zero; Float 1.567 ] ];
    }
  in
  let mat_var2 : Io.matrix_var =
    {
      name = "b";
      matrix =
        [
          [ Rational (1, 1); Zero ];
          [ Rational (6, 3); Rational (2, 1) ];
        ];
    }
  in
  let tree3_matrix_eq : matrix_eq =
    { matrix_lst = [ mat_var1; mat_var2 ]; equ = "3^a+b" }
  in
  parse_matrix_eq tree3_matrix_eq

(** making a tree like tree4 below but using [parse_matrix_eq] *)
let tree4_maker () =
  let mat_var1 : Io.matrix_var =
    {
      name = "a";
      matrix =
        [ [ Float (-1.3); Rational (1, 5) ]; [ Float 7.6; Zero ] ];
    }
  in
  let mat_var2 : Io.matrix_var =
    {
      name = "b";
      matrix =
        [
          [ Rational (-7, 1); Float (-8.9) ];
          [ Rational (6, 5); Rational (2, 1) ];
        ];
    }
  in
  let tree4_matrix_eq : matrix_eq =
    { matrix_lst = [ mat_var1; mat_var2 ]; equ = "   a*4  ^  b" }
  in
  parse_matrix_eq tree4_matrix_eq

(** making a tree like tree5 below but using [parse_matrix_eq] *)
let tree5_maker () =
  let mat_var1 : Io.matrix_var =
    {
      name = "a";
      matrix =
        [ [ Float (-1.3); Rational (1, 5) ]; [ Float 7.6; Zero ] ];
    }
  in
  let mat_var2 : Io.matrix_var =
    {
      name = "b";
      matrix =
        [ [ Float 1.2; Float (-1.3) ]; [ Zero; Rational (1, 1) ] ];
    }
  in
  let mat_var3 : Io.matrix_var =
    {
      name = "c";
      matrix = [ [ Zero; Float 2.8 ]; [ Rational (6, 5); Zero ] ];
    }
  in
  let mat_var4 : Io.matrix_var =
    {
      name = "d";
      matrix =
        [
          [ Rational (-7, 4); Float 0.8 ];
          [ Rational (2, 5); Rational (4, 1) ];
        ];
    }
  in
  let tree5_matrix_eq : matrix_eq =
    {
      matrix_lst = [ mat_var1; mat_var2; mat_var3; mat_var4 ];
      equ = "a*b-c^4.1+d";
    }
  in
  parse_matrix_eq tree5_matrix_eq

(** example tree 1 *)
let tree1 : Io.equ_tree =
  Op_Node
    {
      op = Add;
      left =
        Matrix_Leaf
          [ [ Float 1.4; Rational (4, 3) ]; [ Zero; Float 1.567 ] ];
      right =
        Matrix_Leaf
          [
            [ Rational (1, 1); Zero ];
            [ Rational (7, 3); Rational (2, 1) ];
          ];
    }

(** example tree 2 *)
let tree2 : Io.equ_tree =
  Op_Node
    {
      op = Add;
      left =
        Matrix_Leaf
          [ [ Float 1.4; Rational (4, 3) ]; [ Zero; Float 1.567 ] ];
      right =
        Op_Node
          {
            op = Add;
            left =
              Matrix_Leaf
                [
                  [ Rational (1, 1); Zero ];
                  [ Rational (6, 3); Rational (2, 1) ];
                ];
            right =
              Matrix_Leaf
                [
                  [ Rational (-2, 1); Rational (4, 3) ];
                  [ Float 1.5; Zero ];
                ];
          };
    }

(** example tree 3 *)
let tree3 : Io.equ_tree =
  Op_Node
    {
      op = Add;
      left =
        Op_Node
          {
            op = SMult;
            left = Matrix_Leaf [ [ Rational (3, 1) ] ];
            right =
              Matrix_Leaf
                [
                  [ Float 1.4; Rational (4, 3) ]; [ Zero; Float 1.567 ];
                ];
          };
      right =
        Matrix_Leaf
          [
            [ Rational (1, 1); Zero ];
            [ Rational (6, 3); Rational (2, 1) ];
          ];
    }

(** example tree 4 *)
let tree4 : Io.equ_tree =
  Op_Node
    {
      op = Mult;
      left =
        Matrix_Leaf
          [ [ Float (-1.3); Rational (1, 5) ]; [ Float 7.6; Zero ] ];
      right =
        Op_Node
          {
            op = SMult;
            left = Matrix_Leaf [ [ Rational (4, 1) ] ];
            right =
              Matrix_Leaf
                [
                  [ Rational (-7, 1); Float (-8.9) ];
                  [ Rational (6, 5); Rational (2, 1) ];
                ];
          };
    }

(** example tree 5 *)
let tree5 : Io.equ_tree =
  Op_Node
    {
      op = Sub;
      left =
        Op_Node
          {
            op = Mult;
            left =
              Matrix_Leaf
                [
                  [ Float (-1.3); Rational (1, 5) ]; [ Float 7.6; Zero ];
                ];
            right =
              Matrix_Leaf
                [
                  [ Float 1.2; Float (-1.3) ]; [ Zero; Rational (1, 1) ];
                ];
          };
      right =
        Op_Node
          {
            op = Add;
            left =
              Op_Node
                {
                  op = SMult;
                  left =
                    Matrix_Leaf
                      [ [ Zero; Float 2.8 ]; [ Rational (6, 5); Zero ] ];
                  right = Matrix_Leaf [ [ Float 4.1 ] ];
                };
            right =
              Matrix_Leaf
                [
                  [ Rational (-7, 4); Float 0.8 ];
                  [ Rational (2, 5); Rational (4, 1) ];
                ];
          };
    }

(** expected result of [fold_tree] on [tree1]. i.e. expected result of
    [fold_tree tree1] *)
let tree1_exp_res =
  [ [ Float 2.4; Rational (4, 3) ]; [ Rational (7, 3); Float 3.567 ] ]

(** expected result of [fold_tree] on [tree2]. i.e. expected result of
    [fold_tree tree2] *)
let tree2_exp_res =
  [ [ Float 0.4; Rational (8, 3) ]; [ Float 3.5; Float 3.567 ] ]

(** expected result of [fold_tree] on [tree3]. i.e. expected result of
    [fold_tree tree3] *)
let tree3_exp_res =
  [ [ Float 5.2; Rational (4, 1) ]; [ Rational (2, 1); Float 6.701 ] ]

(** expected result of [fold_tree] on [tree4]. i.e. expected result of
    [fold_tree tree4] *)
let tree4_exp_res =
  [ [ Float 37.36; Float 47.88 ]; [ Float (-212.8); Float (-270.56) ] ]

(** expected result of [fold_tree] on [tree5]. i.e. expected result of
    [fold_tree tree5] *)
let tree5_exp_res =
  [ [ Float 0.19; Float (-10.39) ]; [ Float 3.8; Float (-13.88) ] ]

(* tests parse_matrix *)
let pm_tests =
  [
    pm_test "mixed 2 x 3"
      [
        [ Float 1.4; Rational (4, 3); Zero ];
        [ Zero; Float 1.567; Float 1. ];
      ]
      "[1.4, 4/3, 0; 0/9, 1.567, 1]" 0;
    pm_test "mixed 2 x 3 no brackets"
      [
        [ Float 1.4; Rational (4, 3); Zero ];
        [ Zero; Float 1.567; Float 1. ];
      ]
      "1.4, 4/3, 0; 0/9, 1.567, 1" 0;
    pm_test "3 x 3 of mixed negatives"
      [
        [ Rational (-555, 2); Float (-1.34); Rational (-347, 1) ];
        [ Float (-6.77778); Rational (-3434, 1); Float (-457.) ];
        [ Float (-0.0009); Rational (3, 1413); Rational (-222, 1) ];
      ]
      "[-555/2,-1.34,-347;-6.77778,-3434,-457.0;-0.0009,-3/-1413, -222]"
      0;
    pm_test "mixed 2 x 3 extra brackets"
      [
        [ Float 1.4; Rational (4, 3); Zero ];
        [ Zero; Float 1.567; Float 1. ];
      ]
      "[[1.4, 4/3, 0]; [0/9, 1.567, 1]]" 0;
    pm_test "zeros 2 x 2"
      [ [ Zero; Zero ]; [ Zero; Zero ] ]
      "[0, 0; 0/7, 0.]" 0;
    pm_test "empty 1" [ []; [] ] "[]" 0;
    pm_test "empty 2" [ []; [] ] "" 0;
    pm_test "mixed 2 x 2 with negatives"
      [ [ Zero; Float (-1.56) ]; [ Rational (1, 2); Rational (-1, 2) ] ]
      "[[0/7, -1.56]; [-1/-2, -1/2]]" 0;
    pm_test "throws invalid input" [] "[hello]" 1;
    pm_test "1 x 1 matrix with rat" [ [ Rational (5, 1) ] ] "[5]" 0;
    pm_test "1 x 1 matrix with zero" [ [ Zero ] ] "[[0]]" 0;
    pm_test "1 x 1 matrix with float" [ [ Float (-1.5) ] ] "[-1.5]" 0;
    pm_test "1 x 3 matrix"
      [ [ Rational (5, 1); Float 6.7; Zero ] ]
      "[5, 6.7, 0]" 0;
    pm_test "3 x 1 matrix"
      [ [ Rational (5, 1) ]; [ Float 6.7 ]; [ Zero ] ]
      "[5; 6.7; 0]" 0;
    pm_test "ragged matrix"
      [
        [ Rational (5, 1) ];
        [ Float 6.7; Rational (3, 1); Rational (-457, 1) ];
        [ Zero; Float 3.1415 ];
      ]
      "[5; 6.7, 3, -457   ; 0, 3.1415         ]" 0;
  ]

let basic =
  {
    rows =
      [
        "x\'= 2.5 x+ 3y +36 ";
        " y\' = 4x + 4.9 y + z";
        "z\' =  4y + 2 z + 9.5";
      ];
    vars = [];
    processed_rows = [];
    primes = [];
  }

let unordered =
  {
    rows = [ "x\'= 2.5 x+ 3y "; " y\' = z + 4.9y +4x"; "z\' =  2z +4y" ];
    vars = [];
    processed_rows = [];
    primes = [];
  }

let y =
  {
    rows = [ "x\' = 3y + 2.5x"; "y\' = z+4.9y+4x"; "z\' =  2z+4y" ];
    vars = [];
    processed_rows = [];
    primes = [];
  }

let z =
  {
    rows =
      [ "z\' =  2z+4y"; "x\' = 3y + 2.5x + 1.2345"; "y\' = z+4.9y+4x" ];
    vars = [];
    processed_rows = [];
    primes = [];
  }

let floats =
  {
    rows =
      [
        "z\' =  1.2345z+6.7y";
        "x\' = 9876.543y + 2.5x";
        "y\' = z+4444.4y+5555x";
      ];
    vars = [];
    processed_rows = [];
    primes = [];
  }

let prime_tests =
  [
    prime_tester
      "test whitespace, case where variables are found in alphabetical \
       order and variables are in order in each equation"
      [
        [ "2.5"; "3"; "0"; "36" ];
        [ "4"; "4.9"; "1"; "0" ];
        [ "0"; "4"; "2"; "9.5" ];
      ]
      [ 'x'; 'y'; 'z' ] basic;
    prime_tester "variables are not in order in each equation"
      [
        [ "2.5"; "3"; "0"; "0" ];
        [ "4"; "4.9"; "1"; "0" ];
        [ "0"; "4"; "2"; "0" ];
      ]
      [ 'x'; 'y'; 'z' ] unordered;
    prime_tester
      "variables are found by parser in non-alphabetical order (first \
       variable in first row is y, not x)"
      [
        [ "2.5"; "3"; "0"; "0" ];
        [ "4"; "4.9"; "1"; "0" ];
        [ "0"; "4"; "2"; "0" ];
      ]
      [ 'x'; 'y'; 'z' ] y;
    prime_tester "primes are not in order "
      [
        [ "0"; "4"; "2"; "0" ];
        [ "2.5"; "3"; "0"; "1.2345" ];
        [ "4"; "4.9"; "1"; "0" ];
      ]
      [ 'z'; 'x'; 'y' ] z;
    prime_tester "many floats "
      [
        [ "0"; "6.7"; "1.2345"; "0" ];
        [ "2.5"; "9876.543"; "0"; "0" ];
        [ "5555"; "4444.4"; "1"; "0" ];
      ]
      [ 'z'; 'x'; 'y' ] floats;
  ]

(** test suite for [fold_tree] and [parse_matrix_eq] *)
let ft_pm_tests =
  [
    ft_pm_test "tree1 model" tree1_exp_res tree1;
    ft_pm_test "tree1 using parse" tree1_exp_res (tree1_maker ());
    ft_pm_test "tree2 model" tree2_exp_res tree2;
    ft_pm_test "tree2 using parse" tree2_exp_res (tree2_maker ());
    ft_pm_test "tree3 model" tree3_exp_res tree3;
    ft_pm_test "tree3 using parse" tree3_exp_res (tree3_maker ());
    ft_pm_test "tree4 model" tree4_exp_res tree4;
    ft_pm_test "tree4 using parse" tree4_exp_res (tree4_maker ());
    ft_pm_test "tree5 model" tree5_exp_res tree5;
    ft_pm_test "tree5 using parse" tree5_exp_res (tree5_maker ());
  ]

(* don't change the name, add other test lists to the list as you make
   new test lists *)
let test_list = List.flatten [ pm_tests; prime_tests; ft_pm_tests ]
