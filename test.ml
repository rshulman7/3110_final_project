open OUnit2
open Reals

let real_test name = name >:: fun _ -> assert_equal false false

let reals_tests = [ (* add tests here *) ]

let suite = "test suite for A2" >::: List.flatten [ reals_tests ]

let _ = run_test_tt_main suite
