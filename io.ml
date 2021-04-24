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

let rec string_iter eq str =
  if String.length str > 1 then (
    if Char.code str.[0] >= 97 && Char.code str.[0] <= 122 then
      if str.[1] <> '\'' && not (List.mem str.[0] eq.vars) then
        eq.vars <- str.[0] :: eq.vars
      else if not (List.mem str.[0] eq.primes) then
        eq.primes <- str.[0] :: eq.primes;
    string_iter eq (String.sub str 1 (String.length str - 1)))
  else if String.length str = 1 then
    if
      Char.code str.[0] >= 97
      && Char.code str.[0] <= 122
      && not (List.mem str.[0] eq.vars)
    then eq.vars <- str.[0] :: eq.vars

(** [find_vars eq] finds the variables present in eq.rows and places
    them, each as a character, in eq.vars*)
let find_vars eq =
  List.iter (string_iter eq) eq.rows;
  eq.primes <- List.rev eq.primes;
  eq.vars <- List.sort Stdlib.compare eq.vars

let ops = [ '+'; '-'; '*'; '/'; '='; 'a'; 'b'; 'c'; 'x'; 'y'; 'z' ]

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
                if List.mem x.[!index] ops then continue := false
                else if x.[!index] = ' ' then index := !index - 1
                else (
                  candidate := Char.escaped x.[!index] ^ !candidate;
                  index := !index - 1)
              done;
              if !candidate = "" then row := "1" :: !row
              else row := !candidate :: !row
          | None -> row := "0" :: !row)
        eq.vars;
      let old_rows = eq.processed_rows in
      eq.processed_rows <- List.rev !row :: old_rows)
    eq.rows;
  eq.processed_rows <- List.rev eq.processed_rows

(** [make_rows] populates eq.processed_rows with a list of lists, with
    each list representing the coefficients of a row. *)
let make_rows eq =
  find_vars eq;
  row_iter eq

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
  try List.find (fun x -> Char.code x <= 57 && Char.code x >= 48) c_lst
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

(** parses out a matrix of Reals from a string input. Requires: String
    of numbers with each entry separated by ',' and each row separated
    by ';'. Ex: "[3, 4, 5; 6, 7, 8]" is [3; 4; 5], [6; 7; 8] in matrix
    form *)
let parse_matrix str =
  let row_lst_str = str |> String.trim |> String.split_on_char ';' in
  extract_cols row_lst_str |> matrix_reals

let parse_real = string_to_real
