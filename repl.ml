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
        ^ (if t = [] then "" else ";\n ")
        ^ print_helper t
  in
  "[" ^ print_helper lst_of_lsts ^ "]"

let matrix_answer matrix =
  print_string
    (String.concat ""
       [
         "**************\n";
         multi_printer (Matrix.real_list_list_of_matrix matrix) ^ "\n";
         "************** \n";
       ])

let vector_answer vec =
  print_string
    (String.concat ""
       [
         "**************\n";
         pp_list pp_elt (Vector.to_reals_list vec) ^ "\n";
         "************** \n";
       ])

let rec vector_reprompt () =
  print_string
    "That is an invalid entry. Please make sure to use correct syntax.";
  print_string "> ";
  vector_parser (read_line ())

and vector_parser input =
  try Vector.of_reals_list (List.hd (Io.parse_matrix input))
  with _ -> vector_reprompt ()

let rec matrix_reprompt () =
  print_string
    "That is an invalid entry. Please make sure to use correct syntax.";
  print_string "> ";
  matrix_parser (read_line ())

(* right now this catches any exception. we could have different
   reprompt phrases for different types of exceptions. if you enter
   nothing, it throws List.hd exception*)
and matrix_parser input =
  try Matrix.of_real_list_list (Io.parse_matrix input)
  with _ -> matrix_reprompt ()

let rec real_reprompt () =
  print_string
    "That is an invalid entry. Please make sure to use correct syntax.";
  print_string "> ";
  real_parser (read_line ())

and real_parser input =
  try Io.parse_real input with _ -> real_reprompt ()

type func =
  | TwoMatrix of (Matrix.t -> Matrix.t -> Matrix.t)
  | Scalar of (Reals.t -> Matrix.t -> Matrix.t)
  | Matrix of (Matrix.t -> Matrix.t)
  | MatrixVector of (Matrix.t -> Vector.t -> Vector.t)
  | Quit
  | Help
  | PromptAgain

let rec prompter () =
  print_string
    (String.concat ""
       [
         "Available Functions:";
         " \n 1. Matrix Summation ";
         " \n 2. Matrix Muliplication";
         " \n 3. Scalar Multiplication ";
         "\n\
         \ Type the number of the operation you wish to do. For help, \
          type 'help'. Or, type 'quit' to quit. ";
       ]);
  print_string "> ";
  let option = read_line () in
  let f =
    if option = "1" then TwoMatrix Matrix.sum
    else if option = "2" then TwoMatrix Matrix.multiply
    else if option = "3" then Scalar Matrix.scalar_mult (* help file *)
    else if option = "quit" then Quit
    else if option = "help" then Help
    else PromptAgain
  in
  reader f

and reader f =
  (match f with
  | TwoMatrix func -> (
      print_endline
        "We need to know the two matrices for this operation. Please \
         input the left matrix.";
      let matrix_a = matrix_parser (read_line ()) in
      print_endline "Please input the right matrix";
      let matrix_b = matrix_parser (read_line ()) in
      try matrix_answer (func matrix_a matrix_b)
      with _ ->
        print_string "There was an error. Check matrix dimensions \n";
        prompter ())
  | Scalar func -> (
      print_endline
        "We need to know the matrix for this operation. Please input \
         the matrix.";
      let matrix_a = matrix_parser (read_line ()) in
      print_endline "Please input the scalar value.";
      let scalar = real_parser (read_line ()) in
      try matrix_answer (func scalar matrix_a)
      with _ ->
        print_string "There was an error. Check matrix dimensions \n";
        prompter ())
  | Matrix func -> (
      print_endline
        "We need to know the matrix for this operation. Please input \
         the matrix.";
      let matrix_a = matrix_parser (read_line ()) in
      try matrix_answer (func matrix_a)
      with _ ->
        print_string "There was an error. Check matrix dimensions \n";
        prompter ())
  | MatrixVector func -> (
      print_endline
        "We need to know the matrix for this operation. Please input \
         the matrix.";
      let matrix_a = matrix_parser (read_line ()) in
      print_endline "Please input the vector.";
      let vector = vector_parser (read_line ()) in
      try vector_answer (func matrix_a vector)
      with _ ->
        print_string "There was an error. Check matrix dimensions \n";
        prompter ())
  | Quit ->
      print_endline "Thank you for using ESTR!";
      exit 0
  | Help -> print_endline "The accepted syntax is...."
  | PromptAgain -> ());
  print_string "\n \n";
  prompter ()

(** [()] starts the calculator. *)
let () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to ESTR Equation Solver.\n";
  prompter ()
