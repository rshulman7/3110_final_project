(* TODO: E: - implement : - val parse_size : string -> int * int - val
   num_matrix : string -> int - val parse_matrix : string -> t list -
   catch invalid inputs and call reprompt R: - val function_opt : string
   -> t list - reprompt - getting REPL set up *)

let reprompt = failwith "Unimplemented"

(* turns a string into a char list by splitting the string at every char *)
let rec list_of_string lst str =
  if String.length str = 0 then List.rev lst
  else
    list_of_string (str.[0] :: lst)
      (String.sub str 1 (String.length str - 1))

(* parses first int from a list of single characters *)
(* DELETE: takes in a char list and returns a (int * char list) tuple
   where the int is the first in the list *)
let find_int c_lst =
  try List.find (fun x -> Char.code x <= 57 && Char.code x >= 48) c_lst
  with Not_found -> reprompt

(** take in size input as "n x m" | "n, m" | (n, m) and finds the
    dimensions of the input matrices. Requires: 2 ints contained within
    the input, 0 <= n, m < 10 *)
let parse_size str : int * int =
  let tup = str |> String.trim |> list_of_string [] |> find_int in
  match tup with
  | int_a, t -> ( match find_int t with int_b, t -> (int_a, int_b))

type t = None
