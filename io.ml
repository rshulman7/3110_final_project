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

(** [list_of_string str] turns [str] into a char list by splitting the
    string at every char *)
let list_of_string str =
  let rec help lst str =
    if String.length str = 0 then List.rev lst
    else if str.[0] = ' ' then
      help lst (String.sub str 1 (String.length str - 1))
    else
      help (str.[0] :: lst) (String.sub str 1 (String.length str - 1))
  in
  help [] str

(* [find_int] parses first int from [char_lst], a list of single
   characters *)
let find_int char_lst =
  try List.find (fun x -> is_digit x) char_lst
  with Not_found -> raise Invalid_input

(** [char_to_int] changes characters that represent digits into the
    integer version of those digits. Ex. changes the character '1' into
    the int 1 *)
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

(** [extract_elem str] splits the string of input containing multiple
    numbers separated by commas into a string list, where each entry
    contains a single number. ex. "12, 34, 56" becomes
    ["12"; "34"; "56"] *)
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

(** [extract_rows] turns a list of strings, each representing row of the
    matrix, into a list of list of strings, with each element of an
    inner list representing an entry in a row. *)
let rec extract_rows = function
  | [] -> []
  | [ "[]" ] -> []
  | h :: t -> extract_elem h :: extract_rows t

(** [negate] applies unary negation to the first element of the list. *)
let negate = function h :: t -> (h * -1) :: t | [] -> []

(** converts list of chars which represent ints to list of ints. Ex:
    ['1'; '2'; '3'] -> [1; 2; 3] *)
let int_lst_of_char_lst lst =
  let rec helper acc = function
    | [] -> List.rev acc
    | h :: t ->
        if h = '-' then negate (helper [] t)
        else helper (char_to_int h :: acc) t
  in
  helper [] lst

(** [int_of_int_list] converts a list of ints into the int it represents
    Ex: [1; 2; 3] -> 123 *)
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

(** [decimal_processer num_list] converts [num_list], a list of ints
    representating the decimal part of float, into a float. Ex: [1; 2]
    \-> .12 *)
let decimal_processor post_decimal =
  let rec helper acc num_list digit =
    match num_list with
    | h :: t ->
        helper (acc +. (digit *. Float.of_int h)) t (digit /. 10.)
    | [] -> acc
  in
  helper 0. post_decimal 0.1

(** [flt_pre_decimal lst] converts [lst] representing the part of a
    float to the left of the decimal point to a float. Ex: converts the
    former portion of "3.14" (before the decimal) to 3. by taking in
    ['3'] and converting to 3. *)
let flt_pre_decimal lst =
  lst |> int_lst_of_char_lst |> int_of_int_list |> Float.of_int

(** [flt_post_decimal lst] converts [lst] into a float. It converts the
    part of the float that is after the decimal of the original float.
    Ex: converts the latter portion of "3.14" (after the decimal) to
    0.14 by taking in ['1'; '4'] and converting to 0.14 *)
let flt_post_decimal lst =
  lst |> int_lst_of_char_lst |> decimal_processor

(** [float_of_char_lst lst_char] converts a char list representing a
    float into a float. ex. ['3','.','1','4'] -> 3.14 *)
let float_of_char_lst lst_char =
  let rec help pre_dec_acc lst_char =
    match lst_char with
    | h :: t ->
        if h = '.' then
          let pre_decimal_chars = List.rev pre_dec_acc in
          let positive =
            pre_decimal_chars = [] || List.hd pre_decimal_chars <> '-'
          in
          let pre_decimal = flt_pre_decimal pre_decimal_chars in
          if positive then pre_decimal +. flt_post_decimal t
          else pre_decimal -. flt_post_decimal t
        else help (h :: pre_dec_acc) t
    | [] -> flt_pre_decimal pre_dec_acc
  in
  help [] lst_char

(** [string_to_int str] turns [str] that represents an int into an int.
    ex. "123" -> 123 *)
let string_to_int str =
  str |> list_of_string |> int_lst_of_char_lst |> int_of_int_list

(** [string_to_float str] turns [str] that represents a float into a
    float ex. "12.34" -> 12.34 *)
let string_to_float str = str |> list_of_string |> float_of_char_lst

(** [string_to_rat str] converts [str], which represents a rational
    number, to a [Rational] representing that same number, unless the
    number is 0, in which case it returns [Zero] *)
let string_to_rat str =
  let rat_lst = str |> String.trim |> String.split_on_char '/' in
  let potential_rat =
    ( string_to_int (List.nth rat_lst 0),
      string_to_int (List.nth rat_lst 1) )
  in
  let f = fst potential_rat in
  let s = snd potential_rat in
  if f = 0 then Zero
  else if f < 0 && s < 0 then Rational (-f, -s)
  else Rational potential_rat

(** [string_to_real str] converts [str], representing a real number, to
    (depending on the number) a [Float], [Rational], or [Zero] that
    represents the same number. *)
let string_to_real str =
  if str = "sin" then Sin
  else if str = "cos" then Cos
  else if str = "exp" then Exp
  else if String.contains str '.' then Float (string_to_float str)
  else if String.contains str '/' then string_to_rat str
  else if string_to_int str = 0 then Zero
  else string_to_rat (str ^ "/1")

(* [string reals] takes in a list of string elements and converts into
   list of Reals.t *)
let string_reals = List.map string_to_real

(* [matrix_reals] takes in a matrix of string elements and converts into
   Reals.t list list *)
let matrix_reals lst = List.map string_reals lst

let is_func str =
  if String.length str < 3 then false
  else
    let start = String.sub str 0 3 in
    start = "cos" || start = "sin" || start = "exp"

(** [find_vars_helper eq str] iterates through [str] and adds the
    characters that represent derivatives into [eq.primes] and the
    characters representing variables into [eq.vars]. For example, "x' =
    x + 3y" will add 'x' into [eqs.primes] and it will add 'x' and 'y'
    into [eqs.vars]*)
let rec make_var_list_help eq str =
  if String.length str = 1 then (
    if is_alpha str.[0] then eq.vars <- str.[0] :: eq.vars)
  else if is_func str then
    make_var_list_help eq (String.sub str 3 (String.length str - 3))
  else (
    if is_alpha str.[0] && str.[1] <> '\'' then
      eq.vars <- str.[0] :: eq.vars
    else if is_alpha str.[0] && not (List.mem str.[0] eq.primes) then (
      eq.primes <- str.[0] :: eq.primes;
      eq.vars <- str.[0] :: eq.vars);
    make_var_list_help eq (String.sub str 1 (String.length str - 1)))

(** [find_vars eq] finds the variables and derivatives present in
    [eq.rows] and places them, each as a character, in [eq.vars] and
    [eq.primes], respectively. For example, if [eq.rows] is
    ["x' = 3y"; "y' = y + z"] then it will add 'x' and 'y' into
    [eqs.primes] and it will add 'y' and 'z' into [eqs.vars] *)
let make_var_list eq =
  List.iter (make_var_list_help eq) eq.rows;
  eq.primes <- List.rev eq.primes;
  eq.vars <- List.sort_uniq Stdlib.compare eq.vars;
  eq.rows <- List.map String.trim eq.rows;
  eq.rows <-
    List.sort (fun x y -> Char.code x.[0] - Char.code y.[0]) eq.rows

let ops = [ '+'; '*'; '/'; '=' ]

let find_constant x row =
  let continue = ref true in
  let index = ref (String.length x - 1) in
  let candidate = ref "" in
  while !index >= 0 && !continue do
    if List.mem x.[!index] ops || is_alpha x.[!index] then
      continue := false
    else if x.[!index] = ' ' then index := !index - 1
    else (
      candidate := Char.escaped x.[!index] ^ !candidate;
      index := !index - 1)
  done;
  if !candidate <> "" then row := !candidate :: !row
  else row := "0" :: !row

let find_coefficient x row i =
  let continue = ref true in
  let index = ref (i - 1) in
  let candidate = ref "" in
  while !index >= 0 && !continue do
    if List.mem x.[!index] ops then continue := false
    else if x.[!index] = ' ' then index := !index - 1
    else if x.[!index] = '-' then (
      candidate := Char.escaped x.[!index] ^ !candidate;
      continue := false)
    else (
      candidate := Char.escaped x.[!index] ^ !candidate;
      index := !index - 1)
  done;
  if !candidate = "" then row := "1" :: !row
  else if !candidate = "-" then row := "-1" :: !row
  else row := !candidate :: !row

let rec find_var_in_row rhs row var =
  let i = ref 0 in
  let continue = ref true in
  while !i < String.length rhs && !continue do
    if is_func (String.sub rhs !i (String.length rhs - !i)) then
      i := !i + 3
    else if rhs.[!i] = var then continue := false
    else i := !i + 1
  done;
  if !i < String.length rhs then find_coefficient rhs row !i
  else row := "0" :: !row

(** [row_iter eq] iterates over [eq.rows] to find the coefficients of
    the variables in eq.vars. The coefficients of each row are
    represented by a list, and the coefficients of all the rows are a
    list of lists in [eq.processed_rows] *)
let row_iter eq =
  List.iter
    (fun eq_string ->
      let row = ref [] in
      let i_of_eq_sign = String.index eq_string '=' in
      let rhs =
        String.sub eq_string (i_of_eq_sign + 1)
          (String.length eq_string - i_of_eq_sign - 1)
      in
      List.iter (find_var_in_row rhs row) eq.vars;
      find_constant eq_string row;
      eq.processed_rows <- List.rev !row :: eq.processed_rows)
    eq.rows;
  eq.processed_rows <- List.rev eq.processed_rows

(** [make_rows] populates eq.processed_rows with a list of lists, with
    each list representing the coefficients of a row. *)
let make_rows eq =
  make_var_list eq;
  row_iter eq

(** [parse_matrix str] parses out a Reals.t list list, representing a
    matrix, from input [str]. Requires: String of numbers with each
    entry separated by ',' and each row separated by ';'. Ex:
    "[3, 4, 5; 6, 7, 8]" is [\[3; 4; 5\], \[6; 7; 8\]] in matrix form *)
let parse_matrix str =
  let row_lst_str = str |> String.trim |> String.split_on_char ';' in
  extract_rows row_lst_str |> matrix_reals

let parse_real = string_to_real

(** [eqrows_to_matrix eq] converts [eq.processed_rows] to
    [Reals.t list list] (i.e. a matrix of Reals) *)
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

(** [make_mat_var x] takes string [x] representing a [matrix_var] and
    converts into type [matrix_var]. RI: x represents a [matrix_var]
    (i.e. there is a var name to the left of '=' and a matrix of
    [Reals.t list list] to the right of the '=') *)
let make_mat_var x =
  let split_lst = String.split_on_char '=' x in
  if List.length split_lst <> 2 then
    failwith "Malformed matrix assignment"
  else
    let trim_lst = List.map String.trim split_lst in
    let var_name = List.hd trim_lst in
    let matrix_val = parse_matrix (List.nth trim_lst 1) in
    { name = var_name; matrix = matrix_val }

(** [extract_vars mat_lst] takes [matrix_var list] and extracts variable
    names (i.e. the [name] field of each [matrix_eq] )*)
let rec extract_vars (mat_lst : matrix_var list) name_acc =
  match mat_lst with
  | h :: t -> h.name :: name_acc
  | [] -> List.rev name_acc

(** [is_real equ] returns true if [equ] represents a [Reals.t] else
    false *)
let is_real equ =
  match string_to_real (String.trim equ) with
  | exception _ -> false
  | _ -> true

(** [find_vars equ vars] filters for vars in the previously defined var
    list to see if any match with the variable name in [equ] *)
let find_var equ vars =
  let mat_lst = List.filter (fun x -> x.name = String.trim equ) vars in
  extract_vars mat_lst []

(** [is_var equ vars] returns true if [equ] is itself a predefined
    variable name (previously defined by the user), else false *)
let is_var equ vars = List.length (find_var equ vars) > 0

(** [op_to_char] converts from [operation] to the op it represents in
    [char] form *)
let op_to_char = function
  | Add -> '+'
  | Sub -> '-'
  | Mult -> '*'
  | SMult -> '^'

(** [find_matrix matrix_lst var_lst] finds a given matrix in a
    [matrix_var list] given [var_lst], the list of names each matrix
    corresponds to *)
let find_matrix matrix_lst var_lst =
  if List.length var_lst > 1 then failwith "Invalid variable name"
  else
    let name = List.hd var_lst in
    let pot_matrix = List.filter (fun x -> x.name = name) matrix_lst in
    if List.length pot_matrix < 1 then failwith "No matrix exists"
    else if List.length pot_matrix > 1 then
      failwith "Duplicate matrix declarations"
    else (List.hd pot_matrix).matrix

(** [real_of_str equ vars mat_lst] turns an [equ] of type [string] which
    contains no ops into a [Matrix_Leaf] with the data [equ] holds into
    a [Reals.t list list] carried by the [Matrix_Leaf] *)
let real_of_str equ vars mat_lst =
  if is_var equ vars then
    Matrix_Leaf (find_matrix mat_lst (find_var equ vars))
  else if is_real equ then
    Matrix_Leaf [ [ string_to_real (String.trim equ) ] ]
  else failwith "Invalid Leaf"

(** [real_of_neg_str] turns an [equ] of type [string list] which
    contains no ops and was previously attatched to a unary negation
    into a [Matrix_Leaf] represnting a negative scalar. Creates a
    [Matrix_Leaf] with the data [equ] holds into a [Reals.t list list]
    carried by the [Matrix_Leaf] *)
let real_of_neg_str equ =
  let pot_neg = String.trim (List.nth equ 1) in
  if is_real pot_neg && pot_neg <> "" then
    Matrix_Leaf [ [ string_to_real ("-" ^ pot_neg) ] ]
  else failwith "Invalid Leaf"

(** [find_ops] finds operations in an [equ] string and creates
    [Op_Nodes] from them. Then creates [Matrix_Leaf] when there are no
    operations left in [equ]. Once [equ] is empty an [Empty_Leaf] is
    added as the base case of recursion. Thus constructing an [equ_tree]
    from a string, [equ], which reprsents this tree *)
let rec find_ops equ var_lst mat_lst =
  if String.length equ <> 0 then
    let sub_lst = String.split_on_char '-' equ in
    if List.length sub_lst > 1 then
      Op_Node (create_op_node Sub sub_lst var_lst mat_lst)
    else
      let add_lst = String.split_on_char '+' equ in
      if List.length add_lst > 1 then
        Op_Node (create_op_node Add add_lst var_lst mat_lst)
      else
        let mult_lst = String.split_on_char '*' equ in
        if List.length mult_lst > 1 then
          Op_Node (create_op_node Mult mult_lst var_lst mat_lst)
        else
          let smult_lst = String.split_on_char '^' equ in
          if List.length smult_lst > 1 then
            Op_Node (create_op_node SMult smult_lst var_lst mat_lst)
          else
            let leaf = equ in
            if String.contains leaf '~' then
              let unop_lst = String.split_on_char '~' leaf in
              real_of_neg_str unop_lst
            else real_of_str leaf var_lst mat_lst
  else Empty_Leaf

(** [create_op_node] creates an [op_node] from a current op of type
    [operation] and an equation list, [equ_lst], which was split on the
    current operation's character. The left subtree becomes the first
    element of [equ_lst] which involves a call to [find_ops] to further
    reduce the subtree to eventually reach a leaf. The right subtree
    folds all the other elements of [equ_lst] back into one string,
    reintroducing the current op between the elements of the list.
    [find_ops] is then called on this subtree to eventually reach a
    leaf. *)
and create_op_node curr_op equ_lst var_lst mat_lst =
  {
    op = curr_op;
    left =
      (if List.length equ_lst < 1 then failwith "Empty equation"
      else find_ops (List.hd equ_lst) var_lst mat_lst);
    right =
      (if List.length equ_lst < 2 then failwith "Invalid Op"
      else
        let rt_equ_lst = List.tl equ_lst in
        let folded =
          String.concat (String.make 1 (op_to_char curr_op)) rt_equ_lst
        in
        find_ops folded var_lst mat_lst);
  }

(** [make_tree equ vars mat_lst] makes an [equ_tree] from an [equ] read
    from the repl, [vars], which is a list of the vars that represent
    previously defined matrices, and [matrix_var list] containing the
    matrices *)
let make_tree equ vars mat_lst = find_ops equ vars mat_lst

(** [parse_matrix_eq mat_eq] takes a [matrix_equ] type and turns into a
    [equ_tree]. NOTE: In current implementation will not handle
    parentheses in [matrix_equ.equ] correctly. *)
let parse_matrix_eq (mat_eq : matrix_eq) =
  let eq = mat_eq.equ in
  let var_lst = mat_eq.matrix_lst in
  make_tree eq var_lst mat_eq.matrix_lst

(** [oper_to_matop] takes [operation] used in [Op_Node] to represent an
    operation on matrices and converts into the operation function used
    in [Matrix.ml] *)
let oper_to_matop = function
  | Add -> Matrix.sum
  | Sub -> Matrix.subtract
  | Mult -> Matrix.multiply
  | SMult -> failwith "Invalid op call"

(** [de_tree] converts a tree which just contains a leaf of type
    [Matrix_Leaf] into the [Matrix.t] that every [Matrix_Leaf] holds *)
let de_tree = function
  | Matrix_Leaf mat -> Matrix.of_real_list_list mat
  | _ -> failwith "Invalid scalar mult: invalid leaf"

(** [scalify sc] converts [sc] a 1x1 matrix representing a [Reals.t]
    scalar into a scalar *)
let scalify sc = Matrix.lookup sc (0, 0)

(** [fold_tree tree] folds an [equ_tree] into the order of operations it
    reprsents, calcuates the final matrix, [Reals.t list list], that the
    tree results in upon evaluation *)
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
