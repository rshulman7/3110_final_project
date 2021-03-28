(** This module handles input strings and converts them into inputs
    useable by the rest of the program *)

(** exception thrown for invalid strings passed to parse_matrix *)
exception Invalid_input

(** [parse_size str] converts str representing the dimensions of a
    matrix to the dimensions. Requires: str contains 2 integers *)
val parse_size : string -> int * int

(** [num_matrix str] converts str representing the number of matrices
    (int) to its number. Requires: str contains 1 integer *)
val num_matrix : string -> int

(** [parse_matrix str] converts str representing a matrix to the matrix
    of reals it represents. Requires: str containing numbers within
    square brackets, elements separated by commas, rows separated by
    semicolons. Ex: "[5, 0; 6.7, -457]" -> [Rational (5, 1); Zero];
    [Float 6.7; Rational (-457, 1)] *)
val parse_matrix : string -> Reals.t list list

(** [parse_real str] converts str representing a real value to its real.
    Requires: str represents a real *)
val parse_real : string -> Reals.t
