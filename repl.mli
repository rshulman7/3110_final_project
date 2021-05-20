(** The read evaluate print loop (REPL) for user interaction with math
    operations.*)

val pp_list : ('a -> string) -> 'a list -> string

val matrix_printer : Reals.t list list -> string
