(* TODO: E: - implement : - val parse_size : string -> int * int - val
   num_matrix : string -> int - val parse_matrix : string -> t list -
   catch invalid inputs and call reprompt R: - val function_opt :
   string-> t list - reprompt - getting REPL set up *)

open Reals


exception Invalid_input


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
  | 46 -> raise Decimal_pt
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

let rec charlst_to_num lst_int lst_char = match lst_char with 
  | h :: t -> 
    charlst_to_num (char_to_int h :: lst_int) t 
  | [] -> List.rev lst_int


let int_list_to_num num_list = 
  let reversed_num = List.rev num_list in 
  let rec helper num rev_list digit =
    match rev_list with 
    | h::t -> helper (num + (digit * h)) t (digit * 10)
    | [] -> num
  in helper 0 reversed_num 1
  let decimal_processor num_list = 
    let rec helper num num_list digit =
      match num_list with 
      | h::t -> helper (num +. (digit *. (Float.of_int h))) t (digit /. 10.)
      | [] -> num
    in helper 0. num_list (0.1)
  (*[1;2;3] , [4;5;6]*)

  let combiner (integer,decimal) = 
    Float.of_int (int_list_to_num integer) +. decimal_processor decimal
    
let final_flt_list lst_char = let rec help int_lst lst_char = match lst_char with 
 | h :: t -> if h = '.' then (List.rev (int_lst), charlst_to_num [] t)
 else help (char_to_int h :: int_lst) t
  | [] -> ([],[])

in combiner (help [] lst_char) 

  
  (* turns an int string into an int *)
let str_to_int str = str |> list_of_string [] |> charlst_to_num [] |> (* *********************HOW to easily turn int list to int?? *)

(* turns a float string into a float *)
let str_to_float str = str |> list_of_string [] |> 
  try charlst_to_num [] with 
  | Decimal_pt -> 

(* turns a rational number string to a rational number *)
let str_to_rat str =
  let rat_lst = str |> String.trim |> String.split_on_char '/' in
  (str_to_int (List.hd rat_lst), str_to_int (List.hd (List.rev rat_lst)))

(* converts string representing a real and converts it to a real type. 0
   represents a float type, 1 represents a rational type, 2 represents
   an int (incl. 0) *)
let string_to_real str =
  if String.contains str '.' then str_to_float str
  else if String.contains str '/' then str_to_rat str
  else str_to_int str  (* *********************funct will be returning diff values??*)

(* takes in a list of string elements and converts into list of reals *)
let rec string_reals = function
  | h :: t -> string_to_real h :: string_reals t
  | [] -> []

(* takes in a matrix of string elements and converts into matrix of
   reals *)
let rec matrix_reals = function
  | h :: t -> string_reals h :: matrix_reals t  (* ********************* should I reverse this?? *)
  | [] -> []

(** parses out a matrix of Reals from a string input. Requires: String
    of numbers with each entry separated by ',' and each row separated
    by ';'. Example: "[3, 4, 5; 6, 7, 8]" is [3; 4; 5], [6; 7; 8] in
    matrix form *)
let parse_matrix str =
  let row_lst_str = str |> String.trim |> String.split_on_char ';' in
  extract_cols row_lst_str |> matrix_reals
