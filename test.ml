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

   The strategy for TestMath was to test heavily the early operations in
   modules reals, matrix and vector. For these modules we used a
   combination of glass-box and black box testing. Our primary interest
   was to verify that the outputs of these modules were correct, so we
   often tested 3 cases per function: a trivial case (such as Zero), the
   simple case (analogous to 1), and a general case (analogous to, say,
   13). We also verified that errors were thrown where necessary, so
   that future functions writte wouldn't successfully complete unless
   they passed reasonable inputs to earlier functions. For the later
   modules, namely linAlg and odeSolver, our testing strategy became
   entirely black box. The goal was to verify that the implementations
   succeeded on a collection of representative inputs, resembling both
   edge cases and general cases. For example, for solving an ODE, an
   edge case is an end-time of Zero, another edge case is with a simple
   matrix of coefficients, such as [2,0;1,0], and a general case might
   be a full matrix and a nonzero end time. Affirmative results in all
   these cases was enough for us to feel certain that the solvers work
   in generality. Some testing for the function [eig] was done manually
   using printing, as the eigenvectors that the function produces are
   not normalized in any easy-to-compute way. Their correctness using
   testing was asserted by results from [exact_linear_solver].

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
  >::: List.flatten [ Testmath.test_list; Testio.test_list ]

let _ = run_test_tt_main suite
