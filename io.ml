(* TODO: E: - implement : 
- val parse_size : string -> int * int 
- val
   num_matrix : string -> int 
   
   - val parse_matrix : string -> t list -
   catch invalid inputs and call reprompt 
   
   

   
   R: - val function_opt : string
   -> t list - reprompt - getting REPL set up *)

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
  with Not_found -> reprompt

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
  | _ -> reprompt

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
let num_matrix str = str |> String.trim |> list_of_string [] |> find_int |> char_to_int

(** takes in a *)