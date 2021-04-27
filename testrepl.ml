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

(**let ft_test name exp_matrix input_tree = "[fold_tree] test: " ^ name
   >:: fun _ -> assert_equal ~cmp:matrix_eq ~printer:multi_printer
   exp_matrix (fold_tree input_tree) *)

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
          [ [ Float 1.; Zero ]; [ Rational (7, 3); Float 2. ] ];
    }

(** expected result of [fold_tree] on [tree1]. i.e. expected result of
    [fold_tree tree1] *)
let tree1_exp_res =
  [ [ Float 2.4; Rational (4, 3) ]; [ Rational (7, 3); Float 3.567 ] ]

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
      [ "x\'= 2.5 x+ 3y "; " y\' = 4x + 4.9 y + z"; "z\' =  4y + 2 z" ];
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
    rows = [ "z\' =  2z+4y"; "x\' = 3y + 2.5x"; "y\' = z+4.9y+4x" ];
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
      [ [ "2.5"; "3"; "0" ]; [ "4"; "4.9"; "1" ]; [ "0"; "4"; "2" ] ]
      [ 'x'; 'y'; 'z' ] basic;
    prime_tester "variables are not in order in each equation"
      [ [ "2.5"; "3"; "0" ]; [ "4"; "4.9"; "1" ]; [ "0"; "4"; "2" ] ]
      [ 'x'; 'y'; 'z' ] unordered;
    prime_tester
      "variables are found by parser in non-alphabetical order (first \
       variable in first row is y, not x)"
      [ [ "2.5"; "3"; "0" ]; [ "4"; "4.9"; "1" ]; [ "0"; "4"; "2" ] ]
      [ 'x'; 'y'; 'z' ] y;
    prime_tester "primes are not in order "
      [ [ "0"; "4"; "2" ]; [ "2.5"; "3"; "0" ]; [ "4"; "4.9"; "1" ] ]
      [ 'z'; 'x'; 'y' ] z;
    prime_tester "many floats "
      [
        [ "0"; "6.7"; "1.2345" ];
        [ "2.5"; "9876.543"; "0" ];
        [ "5555"; "4444.4"; "1" ];
      ]
      [ 'z'; 'x'; 'y' ] floats;
  ]

(** test suite for [fold_tree] *)

(** let ft_tests = [ft_test "tree1" tree1_exp_res tree1] *)

(* don't change the name, add other test lists to the list as you make
   new test lists *)
let test_list = List.flatten [ pm_tests; prime_tests ]
