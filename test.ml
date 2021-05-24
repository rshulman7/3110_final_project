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

   TestIo does...

   Parsing: Tests the parsing ability of io.ml which automatically tests
   (using OUnit) [parse_matrix] using typical and boundary cases for
   input strings. Only [parse_matrix] is directly tested by Ounit, the
   rest of the functions used by [parse_matrix] are tested in the call
   to [parse_matrix] as well as manually tested in utop.

   Exceptions potentially thrown by [parse_matrix] or its helpers are
   also automatically tested for since [pm_test] which builds the
   structure for the automated test suite for [parse_matrix] has a
   [exn_bin] argument. If this value is 0, the automated testing will go
   as normal; if this value is 1, there is OUnit testing to catch an
   exception that is expected when setting up the test and input string.

   Differential equations: The differential equation tester
   [prime_tester] was developed using black-box testing. All corner
   cases were investigated: presence/absence of a certain variable,
   variables in order and out of order, equations in "alphabetical
   order" by derivative and out of order, integer/float coefficients or
   no coefficient, sins/cosines/exponentials.

   Equation trees: Tests for the construction and folding of the
   equation trees in io.ml are also in testio.ml. Helper functions were
   tested manually in utop, while [fold_tree] was tested both
   automatically and manually. Construction of an equation tree could
   not be directly tested. However in the test file, trees were both
   made using [parse_matrix_eq] (see treeX_maker) and by manually
   constructing them (see treeX). Then both of these trees (with varying
   methods of construction) were folded and the result was compared to
   the expected result (calculated by hand) based on the equation the
   equation tree represents.

   Thus, the correctness of the fold function alone was tested
   (comparing the fold applied to the manually constructed tree to the
   expected result), and the correctness of the building function was
   tested (by building the tree using [parse_matrix_eq] based on an
   input equation, then folding it, and comparing it to the expected
   result which was the same as the expected result for the manually
   constructed tree. All of these [fold_tree] tests were automatic (with
   the exception of having to build the trees which were done outside of
   the test suite). Typical and edges equations were passed into
   [parse_matrix_eq] and subsequently [fold_tree] to check correctness.

   All expected exceptions thrown by [parse_matrix_eq] and [fold_tree]
   as well as other boundary cases were tested for manually in utop and
   in our REPL (using "make calc"). All other helper functions for
   [parse_matrix_eq] and [fold_tree] were tested manually (stated above)
   or tested in the calls to these two main functions. *)

let suite =
  "test suite for project"
  >::: List.flatten [ TestMath.test_list; TestIo.test_list ]

let _ = run_test_tt_main suite
