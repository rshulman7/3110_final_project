open OUnit2
open Io

(* open Testmath open Testrepl *)

exception Invalid_input

(* checks equality of 2 matrices *)
let matrix_eq mat_a mat_b =
  let lst_a = List.flatten mat_a in
  let lst_b = List.flatten mat_b in
  lst_a = lst_b

(* helper function for io_tests which tests parse_matrix. Tests valid
   input (i.e. no exn raised) if exn_bin = 0; otherwise tests if exn
   raised *)
let pm_test name exp_matrix input_str exn_bin =
  if exn_bin = 0 then
    "[parse_matrix] test: " ^ name >:: fun _ ->
    assert (matrix_eq exp_matrix (parse_matrix input_str))
  else
    "[parse_matrix] exn test: " ^ name >:: fun _ ->
    assert_raises Invalid_input (fun () -> parse_matrix input_str)

(* mainly tests parse_matrix, with a few tests intially to parse_size
   and num_matrix *)
let io_tests =
  [
    pm_test "mixed 2 x 3"
      [
        [ Float 1.4; Rational (4, 3); Zero ];
        [ Zero; Float 1.567; Float 1. ];
      ]
      "[1.4, 4/3, 0; 0/9, 1.567, 1]" 0;
    pm_test "zeros 2 x 2"
      [ [ Zero; Zero ]; [ Zero; Zero ] ]
      "[0, 0; 0/7, 0.]" 0;
  ]

let suite =
  "test suite for project"
  >::: List.flatten [ Testmath.test_list; Testrepl.test_list ]

let _ = run_test_tt_main suite
