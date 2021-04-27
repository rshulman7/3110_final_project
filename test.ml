open OUnit2
open Testrepl

let suite =
  "test suite for project" >::: List.flatten [ Testrepl.test_list ]

let _ = run_test_tt_main suite
