(** This module handles input strings and converts them into inputs
    useable by the rest of the program *)

(** exception thrown for invalid strings passed to parse_matrix *)
exception Invalid_input

type eqs = {
  mutable rows : string list;
  mutable vars : char list;
  mutable primes : char list;
  mutable processed_rows : string list list;
}

type matrix_var = {
  name : string;
  matrix : Reals.t list list;
}

type matrix_eq = {
  matrix_lst : matrix_var list;
  equ : string;
}

type matrix_eq_mut = {
  mutable matrix_lst : matrix_var list;
  mutable equ : string;
}

type operation =
  | Add
  | Sub
  | Mult
  | Div

type equ_tree =
  | Matrix_Leaf of Reals.t list list
  | Op_Node of op_node
  | Empty_Leaf

and op_node = {
  op : operation;
  left : equ_tree;
  right : equ_tree;
}

val make_mat_var : string -> matrix_var

val mat_eqs_fr_mut : matrix_eq_mut -> matrix_eq

val parse_matrix_eq : matrix_eq -> equ_tree

val fold_tree : equ_tree -> Reals.t list list

val eqrows_to_matrix : eqs -> Reals.t list list

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
