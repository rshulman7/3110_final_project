let parser input = "Ellie's parser should return something here"

let mult input input = "Tom and Steve's function will return something"

let rec prompt_next () =
  print_string
    (String.concat ""
       [
         "Available Functions:";
         " \n 1. Solver ";
         " \n 2. Matrix Operations";
         " \n 3. Something Else ";
         "\n\
         \ Type the number of the operation you wish to do. Or, type \
          'quit' to quit. ";
       ]);
  print_string "> ";
  let num = read_line () in
  prompter num

and prompter option =
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
    let result = mult matrix_a matrix_b in
    print_endline result)
  else if option = "2" then print_endline "to do";

  prompt_next ()

(** [main ()] prompts for the game to play, then starts it. *)
and main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to ESTR Equation Solver.\n";
  prompt_next ()

(* Execute the game engine. *)
let () = main ()
