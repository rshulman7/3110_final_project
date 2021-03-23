open Reals

exception Invalid_input

exception Decimal_pt

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
  | _ -> failwith (Char.escaped c)

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
  str |> String.trim
  |> String.split_on_char '['
  |> List.filter (fun x -> x <> "")
  |> List.rev |> List.hd
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
  match lst with h :: t -> extract_elem h :: extract_cols t | [] -> []

(* converts list of chars which represent ints to list of ints. Ex:
   ['1'; '2'; '3'] -> [1; 2; 3] *)
let int_lst_of_char_lst = List.map char_to_int

(* converts a list of ints into the int it represents (by appending each
   int in the list in order onto each other). Ex: [1; 2; 3] -> 123 *)
let int_of_int_list num_list =
  let reversed_num = List.rev num_list in
  let rec helper num rev_list digit =
    match rev_list with
    | h :: t -> helper (num + (digit * h)) t (digit * 10)
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
  lst |> List.rev |> int_lst_of_char_lst |> int_of_int_list
  |> Float.of_int

(* converts list cf chars (once a string rep. a float) into a float.
   converts the float after the decimal of the original float. Ex:
   converts the latter portion of "3.14" (after the decimal) to 0.14 by
   taking in ['1'; '4'] and converting to 0.14 *)
let flt_post_decimal lst =
  lst |> int_lst_of_char_lst |> decimal_processor

(* converts a list of chars (which was once a string representing a
   float) into a float *)
let float_of_char_lst lst_char =
  let rec help int_lst lst_char =
    match lst_char with
    | h :: t ->
        if h = '.' then flt_pre_decimal int_lst +. flt_post_decimal t
        else help (h :: int_lst) t
    | [] -> flt_pre_decimal int_lst
  in
  help [] lst_char

(* turns an int string into an int *)
let string_to_int str =
  str |> list_of_string |> int_lst_of_char_lst |> int_of_int_list

(* turns a float string into a float *)
let string_to_float str = str |> list_of_string |> float_of_char_lst

(* turns a rational number string to a rational number. If numerator of
   potential Rational number is 0, then returns Zero *)
let string_to_rat str =
  let rat_lst = str |> String.trim |> String.split_on_char '/' in
  let potential_rat =
    ( string_to_int (List.hd rat_lst),
      string_to_int (List.hd (List.rev rat_lst)) )
  in
  if fst potential_rat = 0 then Zero else Rational potential_rat

(* converts string representing a real and converts it to a real type *)
let string_to_real str =
  if String.contains str '.' then Float (string_to_float str)
  else if String.contains str '/' then string_to_rat str
  else
    let int_val = string_to_int str in
    if int_val = 0 then Zero else Float (string_to_float str)

(* takes in a list of string elements and converts into list of reals *)
let string_reals = List.map string_to_real

(* takes in a matrix of string elements and converts into matrix of
   reals *)
let rec matrix_reals = List.map string_reals

(** parses out a matrix of Reals from a string input. Requires: String
    of numbers with each entry separated by ',' and each row separated
    by ';'. Ex: "[3, 4, 5; 6, 7, 8]" is [3; 4; 5], [6; 7; 8] in matrix
    form *)
let parse_matrix str =
  let row_lst_str = str |> String.trim |> String.split_on_char ';' in
  extract_cols row_lst_str |> matrix_reals

let parse_real = string_to_real
