open Reals

exception Invalid_input

exception Decimal_pt

(** [type eqs] holds the [rows] of expressions inputted from the repl,
    the [vars] contained in those expressions, and the result of running
    [make_rows] in [processed_rows]*)
type eqs = {
  mutable rows : string list;
  mutable vars : char list;
  mutable primes : char list;
  mutable processed_rows : string list list;
}

(** [is_alpha x] returns true if x is in [A..Z] or [a..z], else false *)
let is_alpha x = Char.code x >= 65 && Char.code x <= 122

(** [is_digit x] returns true if x is in [0..9], else false *)
let is_digit x = Char.code x >= 48 && Char.code x <= 57

(* let row_iter var eq = let col = ref [] in List.iter (fun x -> let i =
   String.index_opt x var in match i with | Some i -> col := if List.mem
   x.[i - 1] ops then '1' :: !col else if x.[i - 1] = ' ' then x.[i - 2]
   :: !col else x.[i - 1] :: !col | None -> col := '0' :: !col) eq.rows;

   let old_rows = eq.processed_cols in eq.processed_cols <- List.rev
   !col :: old_rows

   let process_row eq = List.iter (function x -> row_iter x eq) eq.vars;
   eq.processed_cols <- List.rev eq.processed_cols

   let make_cols eq = find_vars eq; process_row eq *)

(* turns a string into a char list by splitting the string at every char *)
let list_of_string str =
  let rec help lst str =
    if String.length str = 0 then List.rev lst
    else if str.[0] = ' ' then
      help lst (String.sub str 1 (String.length str - 1))
    else
      help (str.[0] :: lst) (String.sub str 1 (String.length str - 1))
  in
  help [] str

(* parses first int from a list of single characters *)
let find_int c_lst =
  try List.find (fun x -> is_digit x) c_lst
  with Not_found -> raise Invalid_input

(* turns int char into int *)
let char_to_int c =
  match Char.code c with
  | 48 -> 0
  | 49 -> 1
  | 50 -> 2
  | 51 -> 3
  | 52 -> 4
  | 53 -> 5
  | 54 -> 6
  | 55 -> 7
  | 56 -> 8
  | 57 -> 9
  | 46 -> raise Decimal_pt
  | _ -> raise Invalid_input

(** take in size input as "n x m" | "n, m" | (n, m) and finds the
    dimensions of the input matrices. Requires: 2 ints contained within
    the input, 0 <= n, m < 10 *)
let parse_size str =
  let dim1 =
    str |> String.trim |> list_of_string |> find_int |> char_to_int
  in
  let dim2 =
    str |> String.trim |> list_of_string |> List.rev |> find_int
    |> char_to_int
  in
  (dim1, dim2)

(** parses number of matrices client wants *)
let num_matrix str =
  str |> String.trim |> list_of_string |> find_int |> char_to_int

(* extracts each element from a string which represents a list of matrix
   elements *)
let extract_elem str =
  let lst =
    str |> String.trim
    |> String.split_on_char '['
    |> List.filter (fun x -> x <> "")
    |> List.rev
  in
  if lst = [] then []
  else
    lst |> List.hd
    |> String.split_on_char ']'
    |> List.filter (fun x -> x <> "")
    |> List.hd
    |> String.split_on_char ','

(* let extract_elem_rs str = let open_bracket = String.index str '[' in
   let close_bracket = String.index str ']' in let len = close_bracket -
   open_bracket in str |> String.trim |> String.sub str ( open_bracket
   +1 ) len *)

(* splits elements of rows into elements of a list *)
let rec extract_cols lst =
  match lst with
  | [] -> []
  | [ "[]" ] -> []
  | h :: t -> extract_elem h :: extract_cols t

(* converts list of chars which represent ints to list of ints. Ex:
   ['1'; '2'; '3'] -> [1; 2; 3] *)

let negate = function h :: t -> (h * -1) :: t | [] -> []

let int_lst_of_char_lst lst =
  let rec helper acc lst =
    match lst with
    | [] -> List.rev acc
    | h :: t ->
        if h = '-' then negate (helper [] t)
        else helper (char_to_int h :: acc) t
  in
  helper [] lst

(* converts a list of ints into the int it represents (by appending each
   int in the list in order onto each other). Ex: [1; 2; 3] -> 123 *)
let int_of_int_list num_list =
  let reversed_num = List.rev num_list in
  let rec helper num rev_list digit =
    match rev_list with
    | h :: t ->
        if h < 0 then -helper (num + (digit * -h)) t (digit * 10)
        else helper (num + (digit * h)) t (digit * 10)
    | [] -> num
  in
  helper 0 reversed_num 1

(* converts a list of ints rep. the decimal part of float into a float.
   Ex: [1; 2] -> .12 *)
let decimal_processor num_list =
  let rec helper num num_list digit =
    match num_list with
    | h :: t ->
        helper (num +. (digit *. Float.of_int h)) t (digit /. 10.)
    | [] -> num
  in
  helper 0. num_list 0.1

(* converts list cf chars (once a string rep. a float) into a float.
   converts the float before the decimal of the original float. Ex:
   converts the former portion of "3.14" (before the decimal) to 3. by
   taking in ['3'] and converting to 3. *)
let flt_pre_decimal lst =
  lst |> int_lst_of_char_lst |> int_of_int_list |> Float.of_int

(* converts list cf chars (once a string rep. a float) into a float.
   converts the float after the decimal of the original float. Ex:
   converts the latter portion of "3.14" (after the decimal) to 0.14 by
   taking in ['1'; '4'] and converting to 0.14 *)
let flt_post_decimal lst =
  lst |> int_lst_of_char_lst |> decimal_processor

(* converts a list of chars (which was once a string representing a
   float) into a float *)
let float_of_char_lst lst_char =
  let rec help pre_dec lst_char =
    match lst_char with
    | h :: t ->
        if h = '.' then
          let pre_decimal_chars = List.rev pre_dec in
          let positive =
            pre_decimal_chars = [] || List.hd pre_decimal_chars <> '-'
          in
          let pre_decimal = flt_pre_decimal pre_decimal_chars in
          if positive then pre_decimal +. flt_post_decimal t
          else pre_decimal -. flt_post_decimal t
        else help (h :: pre_dec) t
    | [] -> flt_pre_decimal pre_dec
  in
  help [] lst_char

(* turns an int string into an int *)
let string_to_int str =
  str |> list_of_string |> int_lst_of_char_lst |> int_of_int_list

(* turns a float string into a float *)
let string_to_float str = str |> list_of_string |> float_of_char_lst

(* turns a rational number string to a rational number. If numerator of
   potential Rational number is 0, then returns Zero. If numerator and
   denominator are both < 0 then negates both (to make a positive
   fraction)*)
let string_to_rat str =
  let rat_lst = str |> String.trim |> String.split_on_char '/' in
  let potential_rat =
    ( string_to_int (List.hd rat_lst),
      string_to_int (List.hd (List.rev rat_lst)) )
  in
  let f = fst potential_rat in
  let s = snd potential_rat in
  if f = 0 then Zero
  else if f < 0 && s < 0 then Rational (-f, -s)
  else Rational potential_rat

(* converts string representing a real and converts it to a real type *)
let string_to_real str =
  if String.contains str '.' then Float (string_to_float str)
  else if String.contains str '/' then string_to_rat str
  else if string_to_int str = 0 then Zero
  else string_to_rat (str ^ "/1")

(* takes in a list of string elements and converts into list of reals *)
let string_reals = List.map string_to_real

(* takes in a matrix of string elements and converts into matrix of
   reals *)
let matrix_reals lst =
  if lst = [] then [ [] ] else List.map string_reals lst

let rec string_iter eq str =
  if String.length str > 1 then (
    if is_alpha str.[0] then
      if str.[1] <> '\'' && not (List.mem str.[0] eq.vars) then
        eq.vars <- str.[0] :: eq.vars
      else if not (List.mem str.[0] eq.primes) then (
        eq.primes <- str.[0] :: eq.primes;
        if not (List.mem str.[0] eq.vars) then
          eq.vars <- str.[0] :: eq.vars );
    string_iter eq (String.sub str 1 (String.length str - 1)) )
  else if String.length str = 1 then
    if is_alpha str.[0] && not (List.mem str.[0] eq.vars) then
      eq.vars <- str.[0] :: eq.vars

(** [find_vars eq] finds the variables present in eq.rows and places
    them, each as a character, in eq.vars*)
let find_vars eq =
  List.iter (string_iter eq) eq.rows;
  eq.primes <- List.rev eq.primes;

  eq.vars <- List.sort Stdlib.compare eq.vars

let ops = [ '+'; '*'; '/'; '=' ]

(** [row_iter eq] iterates over eq.rows to find the coefficients of the
    variables in eq.vars. The coefficients of each row are represented
    by a list, and the coefficients of all the rows are a list of lists
    in eq.processed_rows *)
let row_iter eq =
  List.iter
    (fun x ->
      let row = ref [] in
      List.iter
        (fun var ->
          let i_of_eq = String.index x '=' in
          let after_eq =
            String.sub x (i_of_eq + 1) (String.length x - i_of_eq - 1)
          in
          let i = String.index_opt after_eq var in
          match i with
          | Some i ->
              let continue = ref true in
              let index = ref (i + i_of_eq) in
              let candidate = ref "" in
              while !index >= 0 && !continue do
                if List.mem x.[!index] ops || is_alpha x.[!index] then
                  continue := false
                else if x.[!index] = ' ' then index := !index - 1
                else (
                  candidate := Char.escaped x.[!index] ^ !candidate;
                  index := !index - 1 )
              done;
              if !candidate = "" then row := "1" :: !row
              else if !candidate = "-" then row := "-1" :: !row
              else row := !candidate :: !row
          | None -> row := "0" :: !row)
        eq.vars;
      let constant_finder s =
        let continue = ref true in
        let index = ref (String.length s - 1) in
        let candidate = ref "" in
        while !index >= 0 && !continue do
          if List.mem x.[!index] ops || is_alpha x.[!index] then
            continue := false
          else if x.[!index] = ' ' then index := !index - 1
          else (
            candidate := Char.escaped x.[!index] ^ !candidate;
            index := !index - 1 )
        done;
        if !candidate <> "" then row := !candidate :: !row
        else row := "0" :: !row
      in
      constant_finder x;
      let old_rows = eq.processed_rows in
      eq.processed_rows <- List.rev !row :: old_rows)
    eq.rows;
  eq.processed_rows <- List.rev eq.processed_rows

(** [make_rows] populates eq.processed_rows with a list of lists, with
    each list representing the coefficients of a row. *)
let make_rows eq =
  find_vars eq;
  row_iter eq

(** parses out a matrix of Reals from a string input. Requires: String
    of numbers with each entry separated by ',' and each row separated
    by ';'. Ex: "[3, 4, 5; 6, 7, 8]" is [3; 4; 5], [6; 7; 8] in matrix
    form *)
let parse_matrix str =
  let row_lst_str = str |> String.trim |> String.split_on_char ';' in
  extract_cols row_lst_str |> matrix_reals

let parse_real = string_to_real

(** converts [eq.processed_rows] to [Reals.t list list] (i.e. a matrix
    of Reals) *)
let eqrows_to_matrix eq = eq.processed_rows |> matrix_reals

(** type [matrix_var] holds a name indicator (i.e. a variable name)
    [name] and a matrix [matrix]. e.g.: name = "M"; matrix =
    [\[Float 1.4; Rational (4, 3)\]; \[ Zero; Float 1.567; Float\]] *)
type matrix_var = {
  name : string;
  matrix : Reals.t list list;
}

(** type [matrix_eq] holds a list of matrices [matrix_lst], and an
    equation [equ], represting an equation on preveiously defined
    matrices. e.g.: matrix_lst = [m1; m2; m3]; equ = " 3*m1 + (m2 * m3)"
    where each mi : matrix_var *)
type matrix_eq = {
  matrix_lst : matrix_var list;
  equ : string;
}

(** type [matrix_eq_mut] holds same as type [matrix_eq], but with
    mutable versions of the fields. *)
type matrix_eq_mut = {
  mutable matrix_lst : matrix_var list;
  mutable equ : string;
}

(** type [operation] represents an elementary operation that can be
    carried out on matrices *)
type operation =
  | Add
  | Sub
  | Mult
  | SMult

(** type [equ_tree] represents the equation on matrices as a tree with
    nodes being operations, [Op_Node], and leaves being matrices
    [Matrix_Leaf] or empty [Empty_Leaf] *)
type equ_tree =
  | Matrix_Leaf of Reals.t list list
  | Op_Node of op_node
  | Empty_Leaf

and op_node = {
  op : operation;
  left : equ_tree;
  right : equ_tree;
}

(** converts from type [matrix_eq_mut] to type [matrix_eq] *)
let mat_eqs_fr_mut mat_mut : matrix_eq =
  { matrix_lst = mat_mut.matrix_lst; equ = mat_mut.equ }

(** takes string x representing a [matrix_var] and converts into this
    type. RI: x represents a [matrix_var] (i.e. there is a var name to
    the left of '=' and a matrix of [Reals.t list list] to the right of
    the '=') *)
let make_mat_var x =
  let split_lst = String.split_on_char '=' x in
  if List.length split_lst <> 2 then
    failwith "Malformed matrix assignment"
  else
    let var_name = List.hd split_lst in
    let matrix_val = parse_matrix (List.nth split_lst 1) in
    { name = var_name; matrix = matrix_val }

(** takes [matrix_var list] and extracts variable names (i.e. the [name]
    field of each [matrix_eq] )*)
let rec extract_vars (mat_lst : matrix_var list) name_acc =
  match mat_lst with
  | h :: t -> h.name :: name_acc
  | [] -> List.rev name_acc

(** checks if [string] is itself a [Reals.t] *)
let is_real equ =
  match string_to_real (String.trim equ) with
  | exception _ -> false
  | _ -> true

(** filters for vars in the previously defined var list to see if any
    match with the variable name in [equ] *)
let find_var equ vars = List.filter (fun x -> x = String.trim equ) vars

(** checks if [string] is itself a predefined variable name (previously
    defined by the user) *)
let is_var equ vars = List.length (find_var equ vars) > 0

(** converts from [operation] to the op it represents in [string] form *)
let op_to_str = function
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | SMult -> "^"

(** finds a given matrix in a [matrix_var list] given [var_lst], the
    list of names each matrix corresponds to *)
let find_matrix matrix_lst var_lst =
  if List.length var_lst > 1 then failwith "Invalid variable name"
  else
    let name = List.hd var_lst in
    let pot_matrix = List.filter (fun x -> x.name = name) matrix_lst in
    if List.length pot_matrix < 1 then failwith "No matrix exists"
    else if List.length pot_matrix > 1 then
      failwith "Duplicate matrix declarations"
    else (List.hd pot_matrix).matrix

(** turns an [equ] of type [string] which contains no ops into a
    [Matrix_Leaf] with the data [equ] holds into a [Reals.t list list]
    carried by the [Matrix_Leaf] *)
let real_of_str equ vars mat_lst =
  print_string equ;
  if is_var equ vars then (
    print_string "enter isvar";
    Matrix_Leaf (find_matrix mat_lst (find_var equ vars)) )
  else if is_real equ then (
    print_string "enter ismat";
    Matrix_Leaf [ [ string_to_real (String.trim equ) ] ] )
  else failwith "Invalid Leaf"

(** finds operations in an [equ] string and creates [Op_Nodes] from
    them. Then creates [Matrix_Leaf] when there are no operations left
    in [equ]. Once [equ] is empty an [Empty_Leaf] is added as the base
    case of recursion. Thus constructing an [equ_tree] from a string,
    [equ], which reprsents this tree *)
let rec find_ops equ var_lst mat_lst =
  print_string "start";
  print_string equ;
  let create_op_node curr_op equ_lst =
    {
      op = curr_op;
      left =
        ( if List.length equ_lst < 1 then failwith "Empty equation"
        else find_ops (List.hd equ_lst) var_lst mat_lst );
      right =
        ( if List.length equ_lst < 2 then failwith "Invalid Op"
        else
          let rt_equ_lst = List.tl equ_lst in
          if List.length rt_equ_lst < 2 then
            find_ops (List.hd rt_equ_lst) var_lst mat_lst
          else
            let folded =
              List.fold_left
                (fun x y -> x ^ op_to_str curr_op ^ y)
                (List.hd rt_equ_lst) rt_equ_lst
            in
            find_ops folded var_lst mat_lst );
    }
  in
  if String.length equ <> 0 then (
    let sub_lst = String.split_on_char '-' equ in
    print_string "\nsplit on sub\n";
    if List.length sub_lst > 1 then (
      print_string "\nenter split on sub\n";
      Op_Node (create_op_node Sub sub_lst) )
    else
      let add_lst = String.split_on_char '+' equ in
      print_string "\nsplit on add\n";
      if List.length add_lst > 1 then (
        print_string "\nenter split on add\n";
        Op_Node (create_op_node Add add_lst) )
      else
        let mult_lst = String.split_on_char '*' equ in
        print_string "\nsplit on mult\n";
        if List.length mult_lst > 1 then (
          print_string "\nenter split on mult\n";
          Op_Node (create_op_node Mult mult_lst) )
        else
          let smult_lst = String.split_on_char '^' equ in
          print_string "\nsplit on smult\n";
          if List.length smult_lst > 1 then (
            print_string "\nenter split on smult\n";
            Op_Node (create_op_node SMult smult_lst) )
          else (
            print_string "mat_node";
            real_of_str equ var_lst mat_lst ) )
  else (
    print_string "empty";
    Empty_Leaf )

(** makes an [equ_tree] from an [equ] read from the repl, [vars], which
    is a list of the vars that represent previously defined matrices,
    and [matrix_var list] containing the matrices *)
let make_tree equ vars mat_lst = find_ops equ vars mat_lst

(** takes a [matrix_equ] type and turns into a [equ_tree]. NOTE: In
    current implementation will not handle parentheses in
    [matrix_equ.equ] correctly. *)
let parse_matrix_eq (mat_eq : matrix_eq) =
  let eq = mat_eq.equ in
  let var_lst = extract_vars mat_eq.matrix_lst [] in
  make_tree eq var_lst mat_eq.matrix_lst

(** takes [operation] used in [Op_Node] to represent an operation on
    matrices and converts into the operation function used in
    [Matrix.ml] *)
let oper_to_matop = function
  | Add -> Matrix.sum
  | Sub -> Matrix.subtract
  | Mult -> Matrix.multiply
  | SMult -> failwith "Invalid op call"

(** converts a tree which just contains a leaf of type [Matrix_Leaf]
    into the [Matrix.t] that every [Matrix_Leaf] holds *)
let de_tree = function
  | Matrix_Leaf mat -> Matrix.of_real_list_list mat
  | _ -> failwith "Invalid scalar mult: invalid leaf"

(** converts [sc] a 1x1 matrix representing a [Reals.t] scalar into a
    scalar *)
let scalify sc = Matrix.lookup sc (0, 0)

(** folds an [equ_tree] into the order of operations it reprsents,
    calcuates the final matrix, [Reals.t list list], that the tree
    results in upon evaluation *)
let fold_tree tree =
  let rec fold_tree_help init = function
    | Empty_Leaf -> init
    | Op_Node node ->
        if node.op = SMult then
          let lf = de_tree node.left in
          let rt = de_tree node.right in
          if Matrix.size lf = (1, 1) then
            Matrix.scalar_mult (scalify lf) rt
          else Matrix.scalar_mult (scalify rt) lf
        else
          oper_to_matop node.op
            (fold_tree_help init node.left)
            (fold_tree_help init node.right)
    | Matrix_Leaf mat -> Matrix.of_real_list_list mat
  in
  let init = Matrix.make_matrix 0 0 Zero in
  Matrix.real_list_list_of_matrix (fold_tree_help init tree)
