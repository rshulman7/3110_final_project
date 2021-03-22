open OUnit2
open Testmath
open Testrepl

let suite =
  "test suite for project"
  >::: List.flatten [ Testmath.test_list; Testrepl.test_list ]

let _ = run_test_tt_main suite
