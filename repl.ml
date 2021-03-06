(** The read evaluate print loop (REPL) for user interaction with math
    operations. *)

(** [matrix_answer matrix] converts an abstract [matrix] to a list and
    pretty-prints the matrix. *)
let matrix_answer matrix =
  print_string
    (String.concat ""
       [
         "******************************************\n";
         Matrix.string_of_matrix matrix ^ "\n";
         "******************************************\n";
       ])

(** [vector_answer vec] converts an abstract [vec] to a list and
    pretty-prints the vector. *)
let vector_answer vec =
  print_string
    (String.concat ""
       [
         "\n******************************************\n";
         Vector.string_of_vector vec;
         "\n****************************************** \n";
       ])

(** [vector_reprompt] tells the user that their vector entry was invalid
    and allows them to give a new input. *)
let rec vector_reprompt () =
  print_string
    "That is an invalid entry. Please make sure to use correct syntax.";
  print_string "> ";
  vector_parser (read_line ())

(** [vector_parser] takes user-inputted vector and converts it to a
    Vector.t. If the input is not in proper syntax, it prompts the user
    to enter a new input. *)
and vector_parser input =
  try Vector.of_reals_list (List.hd (Io.parse_matrix input))
  with _ -> vector_reprompt ()

(** [matrix_reprompt] tells the user that their matrix entry was invalid
    and allows them to give a new input. *)
let rec matrix_reprompt () =
  print_string
    "That is an invalid entry. Please make sure to use correct syntax.";
  print_string "> ";
  matrix_parser (read_line ())

(** [matrix_parser] takes user-inputted matrix and converts it to a
    Matrix.t. If the input is not in proper syntax, it prompts the user
    to enter a new input. *)
and matrix_parser input =
  try Matrix.of_real_list_list (Io.parse_matrix input)
  with _ -> matrix_reprompt ()

(** [real_reprompt] tells the user that their real number entry was
    invalid and allows them to give a new input. *)
let rec real_reprompt () =
  print_string
    "That is an invalid entry. Please make sure to use correct syntax.";
  print_string "> ";
  real_parser (read_line ())

(** [real_parser] takes user-inputted float, int, or rational and
    converts it to a Reals.t. If the input is not in proper syntax, it
    prompts the user to enter a new input. *)
and real_parser input =
  try Io.parse_real input with _ -> real_reprompt ()

type func =
  | MatrixVector of (Matrix.t -> Vector.t -> Matrix.t)
  | DiffyQ
  | MatrixOps
  | Plotter
  | Quit
  | Help
  | PromptAgain
      (** [func] constructors represent possible functions to be called
          by the user*)

(** [matrix_help ()] prints instructions for how users should input
    matrices.*)
let matrix_help () =
  print_string
    (String.concat ""
       [
         "\n\
          Matrices: \n\n\
          Values in the same row should be separated by commas (,)";
         "\nRows should be separated by semicolons (;).";
         "\nYou can optionally wrap rows in brackets.";
         "\n\nValid Syntax: a_11, a_12; a_21, a_22";
         "\nValid Syntax: [a_11, a_12]; [a_21, a_22]";
         "\nValid Syntax: [[a_11, a_12]; [a_21, a_22]]";
         "\n\nwhere a_mn is the n_th entry in the m_th row,";
         "\nand it is an integer, a floating point number,";
         "\nor a fraction.\n";
         "\n******************************************\n";
       ])

(** [read_file filename] reads in a text file line-by-line. Note that
    the code is NOT written by the authors of this project and was
    developed via a StackOverFlow solution. Reference:
    https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml/23456034 *)
let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

(** [op_help ()] prints instructions for how users should input
    operations on matrices.*)
let op_help () =
  print_string (String.concat "\n" (read_file "help.txt"))

(** [diffy_q_help ()] prints instructions for how users should input
    differential equations.*)
let diffy_q_help () =
  print_string
    (String.concat ""
       [
         "\nDifferential Equations: \n ";
         "\n\
          Valid Syntax: x' = ax + by + cz + d, where a, b, c, d are \
          constants. Any letter is an acceptable variable.\n";
         "\n******************************************\n";
       ])

(** [plotter_help ()] prints instructions for how users should input
    differential equations.*)
let plotter_help () =
  print_string
    (String.concat ""
       [
         "Note to Windows users: When prompted for file name, please \
          make sure your file name has extension '.ps'\n";
       ])

(** [help ()] returns the help module*)
let help () =
  print_string
    (String.concat ""
       [
         "******************************************\n";
         "Help Module\n";
         "******************************************\n";
       ]);
  matrix_help ();
  op_help ();
  diffy_q_help ()

(** [equation_reader eqs] prompts the user to enter a string of
    differential equations, and stores these strings in [eqs.rows]*)
let equation_reader (eqs : Io.eqs) =
  print_string "Type your first expression and then press enter: ";
  let x = ref (read_line ()) in
  while !x <> "done" do
    let old_rows = eqs.rows in
    eqs.rows <- !x :: old_rows;
    print_string
      "Type another expression and then press enter. Or type 'done': ";
    x := read_line ()
  done;
  eqs.rows <- List.rev eqs.rows

(** [equation_eval eqs] takes the differential equations processed by
    [equation_reader], converts them from strings to Reals.t, and places
    the converted version in [eqs.processed_rows]. It then prints the
    result for the user. *)
let rec equation_eval (eqs : Io.eqs) =
  try
    print_string
      "Here are the coefficients of the right-hand sides of your \
       equations, in alphabetical order of their variable. The last \
       entry in each row is the constant:\n";
    Io.make_rows eqs;
    let matrix = Io.eqrows_to_matrix eqs in
    print_string
      (Matrix.string_of_matrix (Matrix.of_real_list_list matrix));
    print_string "\n******************************************\n"
  with _ ->
    print_string
      "There was an error. Check that you used the correct syntax. \n";
    prompter ()

(** [equations_solver eqs] prompts the user for which type of solver
    they would like to use on [eqs], uses that solver to produce a
    result, and returns the result. *)
and equation_solver eqs =
  let solver_type = ref "" in
  while !solver_type <> "done" do
    print_string
      "\n\
       Proceed with Euler's Method, Runge-Kutta Method, or Exact \
       Solver? Type 'Euler', 'Runge' or 'Exact'. Or 'done' to exit.: \n";
    solver_type := read_line ();
    if !solver_type = "Euler" then step_solver OdeSolver.euler eqs
    else if !solver_type = "Runge" then step_solver OdeSolver.rk eqs
    else if !solver_type = "Exact" then exact_solver eqs
  done

(** [step_solver eqs] asks users for the necessary inputs for solving of
    the system of differential equations [eqs] using step-wise solver
    [solver_type]. It then prints the result. *)
and step_solver solver_type (eqs : Io.eqs) =
  print_string "Enter initial condition as a row vector: ";
  let initial_cond = vector_parser (read_line ()) in
  print_string "Enter end time: ";
  let end_time = real_parser (read_line ()) in
  print_string "Enter step size: ";
  let step_size = real_parser (read_line ()) in
  let matrix = Matrix.of_real_list_list (Io.eqrows_to_matrix eqs) in
  print_string "Result: ";
  try
    vector_answer
      (solver_type true matrix initial_cond end_time step_size)
  with _ ->
    print_string "There was an error. \n";
    prompter ()

(** [exact_solver eqs] asks users for the necessary inputs for exact
    solving of the system of differential equations [eqs] and then
    prints the result. *)
and exact_solver (eqs : Io.eqs) =
  print_string "Enter initial condition as a row vector: ";
  let initial_cond = vector_parser (read_line ()) in
  print_string "Enter end time: ";
  let end_time = real_parser (read_line ()) in
  let matrix = Matrix.of_real_list_list (Io.eqrows_to_matrix eqs) in
  print_string "Result: ";
  try
    vector_answer
      (OdeSolver.exact_linear_solver matrix initial_cond end_time)
  with _ ->
    print_string "There was an error. \n";
    prompter ()

and print_available_functions () =
  print_string
    (String.concat ""
       [
         "Available Functions:";
         " \n 1. General Matrix Operations";
         " \n 2. Row Reduction (Gaussian Elimination) ";
         " \n 3. Differential Equation Solver ";
         " \n 4. Plotter ";
         "\n\
         \ Type the number of the operation you wish to do. For help, \
          type 'help'. Or, type 'quit' to quit. ";
       ])

(** [prompter] informs the user about available operations, reads their
    choice of operation, and then calls [reader] to request further
    information about that choice *)
and prompter () =
  print_available_functions ();
  print_string "> ";
  let option = read_line () in
  let f =
    if option = "1" then MatrixOps
    else if option = "2" then MatrixVector LinAlg.rref
    else if option = "3" then DiffyQ
    else if option = "4" then Plotter
    else if option = "quit" then Quit
    else if option = "help" then Help
    else PromptAgain
  in
  reader f

and matrix_read func =
  print_endline "To solve Ax=b, please input the matrix 'A': ";
  let matrix = matrix_parser (read_line ()) in
  print_endline "Please input the vector 'b', as a row vector: ";
  let vector = vector_parser (read_line ()) in
  try matrix_answer (func matrix vector)
  with _ ->
    print_string "There was an error. Check matrix dimensions. \n";
    prompter ()

and diffy_q_read () =
  diffy_q_help ();
  let eqs : Io.eqs =
    { rows = []; vars = []; processed_rows = []; primes = [] }
  in
  equation_reader eqs;
  equation_eval eqs;
  equation_solver eqs

and plotter_read () =
  plotter_help ();
  print_string "Please enter a 2 x n matrix: ";
  let matrix = matrix_parser (read_line ()) in
  try Plot.make_plot matrix
  with _ ->
    print_string "There was an error. Check matrix dimensions. \n";
    prompter ()

and ops_read () =
  print_string
    "Type your first matrix and assign it a name. For example, ' a = \
     [1,2;3,4] '. Then press enter. ";
  try tree_builder ()
  with _ ->
    print_string
      "\n\
       There was an error. Check that you used the correct syntax and \
       that your matrix dimensions are correct. \n\
       Type \'help\' for more information.\n\n";
    prompter ()

(** [reader f] prompts user for inputs that are appropriate for function
    [f] and returns the result of calling [f] on those inputs. *)
and reader f =
  (match f with
  | MatrixVector f -> matrix_read f
  | DiffyQ -> diffy_q_read ()
  | Plotter -> plotter_read ()
  | MatrixOps -> ops_read ()
  | Quit ->
      print_endline "Thank you for using ESTR!";
      exit 0
  | Help -> help ()
  | PromptAgain -> ());
  print_string "\n \n";
  prompter ()

(** [tree_builder] is a REPL that reads matrices with user-defined
    names, applies user-defined operations to them, and then prints the
    answer.*)
and tree_builder () =
  let mat_eq : Io.matrix_eq_mut = { matrix_lst = []; equ = "" } in
  let x = ref (read_line ()) in
  while String.trim !x <> "done" do
    let old_lst = mat_eq.matrix_lst in
    mat_eq.matrix_lst <- Io.make_mat_var !x :: old_lst;
    print_string
      "Type another matrix and assign it a name; then\n\
       press enter. Or type 'done'. ";
    x := read_line ()
  done;
  mat_eq.matrix_lst <- List.rev mat_eq.matrix_lst;
  print_string
    "Type your equation using the matrix variables defined above.\n\
     Then press enter. ";
  mat_eq.equ <- read_line ();
  let tree = Io.parse_matrix_eq (Io.mat_eqs_fr_mut mat_eq) in
  matrix_answer (Matrix.of_real_list_list (Io.fold_tree tree))

(** [()] starts the calculator. *)
let () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to ESTR Equation Solver.\n";
  prompter ()
