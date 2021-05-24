open OUnit2

(* Similar to A2's REPL, we were unable to write test cases to test the
   REPL itself. Instead, we ran the REPL using [make calc] and manually
   explored as many paths through the REPL as possible. For example, one
   path would be choosing the Differential Equations solver, then
   entering one equation, then using the Euler solver, and then using
   the Exact solver. Using our knowledge that the REPL's differential
   equation prompts were written with while-loops ("glass box"), we
   could rest assured that if Euler solver followed by Exact solver
   worked, then any sequences of solvers would work as long as each
   individual solver worked. The fact that, on each path, the REPL took
   in data, processed it, and gave an answer without crashing gave us
   confidence that it was working correctly, as long as the functions it
   was calling were giving correct output for our input. Thus, it then
   sufficed to test the functions in other modules that were called by
   the REPL.

   The modules used by the REPL are tested in [Testmath.test_list] and
   [Testio.test_list], and [test.ml] tests both of those, allowing the
   grader to call [make test] just once. We created two modules for
   testing to reflect the separate nature of the parsing functions and
   the mathematical functions, and the division of work between our
   team.

   Testmath does ....

   Testio does... The differential equation tester [prime_tester] was
   developed using black-box testing. All corner cases were
   investigated: presence/absence of a certain variable, variables in
   order and out of order, equations in "alphabetical order" by
   derivative and out of order, integer/float coefficients or no
   coefficient, sins/cosines/exponentials.*)

(* However, describing your approach to testing: what you tested,
   anything you omitted testing, and why you believe that your test
   suite demonstrates the correctness of your system. A detailed rubric
   can be found below.

   -1: The test plan does not explain which parts of the system were
   automatically tested by OUnit vs. manually tested. -1: The test plan
   does not explain what modules were tested by OUnit and how test cases
   were developed (black box, glass box, , etc.). -1: The test plan does
   not provide an argument for why the testing approach demonstrates the
   correctness of the system. *)

let suite =
  "test suite for project"
  >::: List.flatten [ TestMath.test_list; TestIo.test_list ]

let _ = run_test_tt_main suite
