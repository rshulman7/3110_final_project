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
  matrix_parser (read_line ())

(* right now this catches any exception. we could have different
   reprompt phrases for different types of exceptions. if you enter
   nothing, it throws List.hd exception*)
and matrix_parser input =
  try Matrix.of_real_list_list (Io.parse_matrix input)
  with _ -> reprompt ()

let rec real_reprompt () =
  print_string
    "That is an invalid entry. Please make sure to use correct syntax.";
  print_string "> ";
  real_parser (read_line ())

(* right now this catches any exception. we could have different
   reprompt phrases for different types of exceptions. if you enter
   nothing, it throws List.hd exception*)
and real_parser input =
  try Io.parse_real input with _ -> real_reprompt ()

let rec prompter () =
  print_string
    (String.concat ""
       [
         "Available Functions:";
         " \n 1. Matrix Summation ";
         " \n 2. Matrix Muliplication";
         " \n 3. Scalar Multiplication ";
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
  else if option = "1" || option = "2" then (
    print_endline
      "We need to know the two matrices for this operation. Please \
       input the left matrix.";
    let matrix_a = matrix_parser (read_line ()) in
    print_endline "Please input the right matrix";
    let matrix_b = matrix_parser (read_line ()) in
    let result =
      try
        ignore
          (if option = "1" then Matrix.sum matrix_a matrix_b
           (* change to multiply once Matrix module compiles*)
          else Matrix.sum matrix_a matrix_b)
      with _ -> prompter ()
    in
    (* switch from matrix_a to result once the math functions are done*)
    print_string
      (String.concat ""
         [
           "**************\n";
           multi_printer (Matrix.real_list_list_of_matrix matrix_a)
           ^ "\n";
           "************** \n";
         ]))
  else if option = "3" then (
    print_endline
      "We need to know the matrix for this operation. Please input the \
       matrix.";
    let matrix_a = matrix_parser (read_line ()) in
    print_endline "Please input the scalar value.";
    let scalar = Io.parse_real (read_line ()) in
    let result =
      try ignore (Matrix.scalar_mult scalar matrix_a)
      with _ -> prompter ()
    in
    (* switch from matrix_a to result once the math functions are done*)
    print_string
      (String.concat ""
         [
           "**************\n";
           multi_printer (Matrix.real_list_list_of_matrix matrix_a)
           ^ "\n";
           "************** \n";
         ]));
  print_string "\n \n";
  ignore (prompter ())

(** [()] starts the calculator. *)
let () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to ESTR Equation Solver.\n";
  prompter ()
