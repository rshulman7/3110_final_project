(* TODO: E: - implement : - val parse_size : string -> int * int - val
   num_matrix : string -> int - val parse_matrix : string -> t list -
   catch invalid inputs and call reprompt R: - val function_opt :
   string-> t list - reprompt - getting REPL set up *)

open Reals

exception Invalid_input

let reprompt = failwith "Unimplemented"

(* turns a string into a char list by splitting the string at every char *)
let rec list_of_string lst str =
  if String.length str = 0 then List.rev lst
  else
    list_of_string (str.[0] :: lst)
      (String.sub str 1 (String.length str - 1))

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
  | _ -> raise Invalid_input

(** take in size input as "n x m" | "n, m" | (n, m) and finds the
    dimensions of the input matrices. Requires: 2 ints contained within
    the input, 0 <= n, m < 10 *)
let parse_size str =
  let dim1 =
    str |> String.trim |> list_of_string [] |> find_int |> char_to_int
  in
  let dim2 =
    str |> String.trim |> list_of_string [] |> List.rev |> find_int
    |> char_to_int
  in
  (dim1, dim2)

(** parses number of matrices client wants *)
let num_matrix str =
  str |> String.trim |> list_of_string [] |> find_int |> char_to_int

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

(* recusrively splits elements of rows into elements of a list *)
let rec extract_cols lst =
  match lst with h :: t -> extract_elem h :: extract_cols t | [] -> []

(* turns a float string into a float *)
let str_to_float str = failwith "u"

(* turns a rational number string to a rational number *)
let str_to_rat str = failwith "u"

(* turns an int string into an int *)
let str_to_int str = failwith "u"

(* converts string representing a real and converts it to a real type. 0
   represents a float type, 1 represents a rational type, 2 represents
   an int (incl. 0) *)
let string_to_real str =
  if String.contains str '.' then str_to_float str
  else if String.contains str '/' then str_to_rat str
  else str_to_int str

(* takes in a list of string elements and converts into list of reals *)
let rec string_reals = function
  | h :: t -> string_to_real h :: string_reals t
  | [] -> []

(* takes in a matrix of string elements and converts into matrix of
   reals *)
let rec matrix_reals = function
  | h :: t -> string_reals h :: matrix_reals t
  | [] -> []

(** parses out a matrix of Reals from a string input. Requires: String
    of numbers with each entry separated by ',' and each row separated
    by ';'. Example: "[3, 4, 5; 6, 7, 8]" is [3; 4; 5], [6; 7; 8] in
    matrix form *)
let parse_matrix str =
  let row_lst_str = str |> String.trim |> String.split_on_char ';' in
  extract_cols row_lst_str |> matrix_reals
