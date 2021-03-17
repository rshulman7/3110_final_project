(** Tests for modules:

    - Math
    - Vector
    - Matrix *)

open OUnit2

(* open your modules*)
let some_test name f args expected =
  name >:: fun _ -> assert_equal expected (f args)

let some_tests = [ (* add tests here *) ]

(* don't change the name, add other test lists to the list as you make
   new test lists *)
let test_list = List.flatten [ some_tests ]
