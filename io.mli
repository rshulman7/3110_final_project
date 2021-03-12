(* float or float list *)
type t 

val function_opt : string -> t list 

val parse_size : string -> int * int

val num_matrix : string -> int

val parse_matrix : string -> t list

val print_matrix : t list -> string