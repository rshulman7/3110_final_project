let rec reprompt () =
  print_string
    "That is an invalid entry. Please make sure to use correct syntax.";
  print_string "> ";
  parser (read_line ())

(* right now this catches any exception. we could have different
   reprompt phrases for different types of exceptions. if you enter
   nothing, it throws List.hd exception*)
and parser input = try Io.parse_matrix input with _ -> reprompt ()

let rec prompter () =
  print_string
    (String.concat ""
       [
         "Available Functions:";
         " \n 1. Matrix Multiplication ";
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
      "To do Matrix Multiplication, we need to know your two matrices. \
       Please input matrix one";
    let matrix_a = parser (read_line ()) in
    print_endline "Please input matrix two";
    let matrix_b = parser (read_line ()) in
    let result = "Placeholder" in
    print_endline result)
  else if option = "2" then print_endline "to do";
  print_string "\n \n \n";
  ignore (prompter ())

(** [()] starts the calculator. *)
let () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to ESTR Equation Solver.\n";
  prompter ()
