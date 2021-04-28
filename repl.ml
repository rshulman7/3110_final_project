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

(** [matrix_printer lst_of_lsts] pretty prints lists of lists, where
    each element of the inner lists is of the type printed by [pp_elt]*)
let matrix_printer lst_of_lsts =
  let rec print_helper = function
    | [] -> ""
    | h :: t ->
        pp_list Reals.string_of_real h
        ^ (if t = [] then "" else ";\n ")
        ^ print_helper t
  in
  "[" ^ print_helper lst_of_lsts ^ "]"

(** [matrix_answer matrix] pretty-prints matrices. *)
let matrix_answer matrix =
  print_string
    (String.concat ""
       [
         "**************\n";
         matrix_printer (Matrix.real_list_list_of_matrix matrix) ^ "\n";
         "************** \n";
       ])

(* [vector_answer vec] pretty-prints vectors *)
let vector_answer vec =
  print_string
    (String.concat ""
       [
         "**************\n";
         pp_list Reals.string_of_real (Vector.to_reals_list vec) ^ "\n";
         "************** \n";
       ])

(* [vector_reprompt] tells the user that their vector entry was invalid
   and allows them to give a new input. *)
let rec vector_reprompt () =
  print_string
    "That is an invalid entry. Please make sure to use correct syntax.";
  print_string "> ";
  vector_parser (read_line ())

(* [vector_parser] takes user-inputted vector and converts it to a
   Vector.t. If the input is not in proper syntax, it prompts the user
   to enter a new input. *)
and vector_parser input =
  try Vector.of_reals_list (List.hd (Io.parse_matrix input))
  with _ -> vector_reprompt ()

(* [matrix_reprompt] tells the user that their matrix entry was invalid
   and allows them to give a new input. *)
let rec matrix_reprompt () =
  print_string
    "That is an invalid entry. Please make sure to use correct syntax.";
  print_string "> ";
  matrix_parser (read_line ())

(* [matrix_parser] takes user-inputted matrix and converts it to a
   Matrix.t. If the input is not in proper syntax, it prompts the user
   to enter a new input. *)
and matrix_parser input =
  try Matrix.of_real_list_list (Io.parse_matrix input)
  with _ -> matrix_reprompt ()

(* [real_reprompt] tells the user that their real number entry was
   invalid and allows them to give a new input. *)
let rec real_reprompt () =
  print_string
    "That is an invalid entry. Please make sure to use correct syntax.";
  print_string "> ";
  real_parser (read_line ())

(* [real_parser] takes user-inputted float, int, or rational and
   converts it to a Reals.t. If the input is not in proper syntax, it
   prompts the user to enter a new input. *)
and real_parser input =
  try Io.parse_real input with _ -> real_reprompt ()

type func =
  | TwoMatrix of (Matrix.t -> Matrix.t -> Matrix.t)
  | Scalar of (Reals.t -> Matrix.t -> Matrix.t)
  | Matrix of (Matrix.t -> Matrix.t)
  | MatrixVector of (Matrix.t -> Vector.t -> Matrix.t)
  | DiffyQ
  | MatrixOps
  | Plotter
  | Quit
  | Help
  | PromptAgain
      (** [func] constructors represent possible functions to be called
          by the user*)

(** [help ()] returns the help module*)
let help () =
  print_string
    (String.concat ""
       [
         "******************************************\n";
         "Help Module\n";
         "******************************************\n";
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
         "\nDifferential Equations: \n ";
         "\n\
          Valid Syntax: x' = ax + by + cz , where a, b, c are \
          constants. Any letter is an acceptable variable.\n";
         "\n******************************************\n";
       ])

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
  print_string "Here are your equations:\n";
  try
    Io.make_rows eqs;
    let matrix = Io.eqrows_to_matrix eqs in
    print_string (matrix_printer matrix)
  with _ ->
    print_string
      "There was an error. Check that you used the correct syntax. \n";
    prompter ()

(** [eulers eqs] asks users for the necessary inputs for Euler's
    equation and then prints results*)
and eulers (eqs : Io.eqs) =
  print_string "\nProceed with Euler's Method? Y/N: \n";
  print_string
    "Please note that only One Dimensional Linear ODEs are supported \
     right now. \n";
  if read_line () = "Y" then (
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
        (Euler.sing_eq_euler matrix initial_cond end_time step_size)
    with _ ->
      print_string "There was an error. \n";
      prompter ())

(** [prompter] informs the user about available operations, reads their
    choice of operation, and then calls [reader] to request further
    information about that choice *)
and prompter () =
  print_string
    (String.concat ""
       [
         "Available Functions:";
         " \n 1. Matrix Summation ";
         " \n 2. Matrix Muliplication";
         " \n 3. Scalar Multiplication ";
         " \n 4. Row Reduction (Gaussian Elimination) ";
         " \n 5. Differential Equation Solver ";
         " \n 6. General Matrix Operations";
         " \n 7. Plotter ";
         "\n\
         \ Type the number of the operation you wish to do. For help, \
          type 'help'. Or, type 'quit' to quit. ";
       ]);
  print_string "> ";
  let option = read_line () in
  let f =
    if option = "1" then TwoMatrix Matrix.sum
    else if option = "2" then TwoMatrix Matrix.multiply
    else if option = "3" then Scalar Matrix.scalar_mult
    else if option = "4" then MatrixVector Linearalgops.rref
    else if option = "5" then DiffyQ
    else if option = "6" then MatrixOps
    else if option = "7" then Plotter
    else if option = "quit" then Quit
    else if option = "help" then Help
    else PromptAgain
  in
  reader f

(** [reader f] prompts user for inputs that are appropriate for function
    [f] and returns the result of calling [f] on those inputs. *)
and reader f =
  (match f with
  | TwoMatrix func -> (
      print_endline
        "We need to know the two matrices for this operation. Please \
         input the left matrix:";
      let matrix_a = matrix_parser (read_line ()) in
      print_endline "Please input the right matrix:";
      let matrix_b = matrix_parser (read_line ()) in
      try matrix_answer (func matrix_a matrix_b)
      with _ ->
        print_string "There was an error. Check matrix dimensions. \n";
        prompter ())
  | Scalar func -> (
      print_endline
        "We need to know the matrix for this operation. Please input \
         the matrix: ";
      let matrix_a = matrix_parser (read_line ()) in
      print_endline "Please input the scalar value: ";
      let scalar = real_parser (read_line ()) in
      try matrix_answer (func scalar matrix_a)
      with _ ->
        print_string "There was an error. Check matrix dimensions \n";
        prompter ())
  | Matrix func -> (
      print_endline
        "We need to know the matrix for this operation. Please input \
         the matrix: ";
      let matrix_a = matrix_parser (read_line ()) in
      try matrix_answer (func matrix_a)
      with _ ->
        print_string "There was an error. Check matrix dimensions \n";
        prompter ())
  | MatrixVector func -> (
      print_endline
        "We need to know the matrix for this operation. Please input \
         the matrix: ";
      let matrix_a = matrix_parser (read_line ()) in
      print_endline "Please input the vector: ";
      let vector = vector_parser (read_line ()) in
      try matrix_answer (func matrix_a vector)
      with _ ->
        print_string "There was an error. Check matrix dimensions. \n";
        prompter ())
  | DiffyQ ->
      let eqs : Io.eqs =
        { rows = []; vars = []; processed_rows = []; primes = [] }
      in
      equation_reader eqs;
      equation_eval eqs;
      eulers eqs
  | Plotter -> (
      print_string "Please enter a 2 x n matrix: ";
      let matrix_to_plot = matrix_parser (read_line ()) in
      try Plot.make_plot matrix_to_plot
      with _ ->
        print_string "There was an error. Check matrix dimensions. \n";
        prompter ())
  | MatrixOps ->
      (*print_string "Type your first matrix and assign it a name. Then
        press enter."; let mat_eq : Io.matrix_eq_mut = { matrix_lst =
        []; equ = "" } in let x = ref (read_line ()) in while !x <>
        "done" do let old_lst = mat_eq.matrix_lst in try
        mat_eq.matrix_lst <- Io.make_mat_var !x :: old_lst with _ ->
        print_string "There was an error. Check that you used the
        correct \ syntax. \n";

        print_string "Type another matrix and assign it a name; then
        press \ enter. Or type 'done'"; x := read_line () done;

        mat_eq.matrix_lst <- List.rev mat_eq.matrix_lst; print_string
        "Type your equation using the matrix variables defined above. \
        Then press enter. "; mat_eq.equ <- read_line (); (*let tree =
        Io.parse_matrix_eq (mat_eqs_fr_mut mat_eq) in tree (** use
        Io.fold_tree to turn tree into final calc matrix *)*) *)
      print_string
        "\n\
        \ The functionality of Matrix Operations is still under works. \n\
        \ \n";
      prompter ()
  | Quit ->
      print_endline "Thank you for using ESTR!";
      exit 0
  | Help -> help ()
  | PromptAgain -> ());
  print_string "\n \n";
  prompter ()

(** [()] starts the calculator. *)
let () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to ESTR Equation Solver.\n";
  prompter ()
