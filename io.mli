(** exception thrown for invalid strings passed to parse_matrix*)
exception Invalid_input

(** converts a string input representing the dimensions of a matrix to
    the dimensions *)
val parse_size : string -> int * int

(** converts a string input representing the number of a matrices (int)
    to the number it represents *)
val num_matrix : string -> int

(** converts a string input representing a matrix to the matrix of reals
    it represents *)
val parse_matrix : string -> Reals.t list list

(** converts a string input representing a real value to the real value
    itself *)
val parse_real : string -> Reals.t
