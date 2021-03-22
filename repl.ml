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

let rec reprompt () =
  print_string
    "That is an invalid entry. Please make sure to use correct syntax.";
  print_string "> ";
  parser (read_line ())

(* right now this catches any exception. we could have different
   reprompt phrases for different types of exceptions. if you enter
   nothing, it throws List.hd exception*)
and parser input =
  try Matrix.of_real_list_list (Io.parse_matrix input)
  with _ -> reprompt ()

let rec prompter () =
  print_string
    (String.concat ""
       [
         "Available Functions:";
         " \n 1. Matrix Summation ";
         " \n 2. Solver";
         " \n 3. Something Else ";
         "\n\
         \ Type the number of the operation you wish to do. Or, type \
          'quit' to quit. ";
       ]);
  print_string "> ";
  let option = read_line () in
  reader option

and reader option =
  if option = "quit" then (
    print_endline "Thank you for using ESTR!";
    exit 0)
  else if option = "1" then (
    print_endline
      "To do Matrix Summation, we need to know your two matrices. \
       Please input matrix one";
    let matrix_a = parser (read_line ()) in
    print_endline "Please input matrix two";
    let matrix_b = parser (read_line ()) in
    let result = Matrix.sum matrix_a matrix_b in
    print_endline
      (multi_printer (Matrix.real_list_list_of_matrix matrix_a)))
  else if option = "2" then print_endline "to do";
  print_string "\n \n \n";
  ignore (prompter ())

(** [()] starts the calculator. *)
let () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to ESTR Equation Solver.\n";
  prompter ()
