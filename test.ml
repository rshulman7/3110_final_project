open OUnit2

(* W describing your approach to testing: what you tested, anything you
   omitted testing, and why you believe that your test suite
   demonstrates the correctness of your system. A detailed rubric can be
   found below.

   -1: The test plan does not explain which parts of the system were
   automatically tested by OUnit vs. manually tested. -1: The test plan
   does not explain what modules were tested by OUnit and how test cases
   were developed (black box, glass box, , etc.). -1: The test plan does
   not provide an argument for why the testing approach demonstrates the
   correctness of the system. *)

let suite =
  "test suite for project"
  >::: List.flatten [ Testmath.test_list; Testrepl.test_list ]

let _ = run_test_tt_main suite
