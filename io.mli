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

(** [matrix_var] holds a name indicator, [name], and a matrix [matrix] *)
type matrix_var = {
  name : string;
  matrix : Reals.t list list;
}

(** [matrix_eq] holds a list of matrices, [matrix_lst], and an equation,
    [equ], represting an equation on preveiously defined matrices *)
type matrix_eq = {
  matrix_lst : matrix_var list;
  equ : string;
}

(** [matrix_eq_mut] is same as type [matrix_eq], but with mutable
    versions of the fields. *)
type matrix_eq_mut = {
  mutable matrix_lst : matrix_var list;
  mutable equ : string;
}

(** [operation] represents an elementary operation that can be carried
    out on matrices *)
type operation =
  | Add
  | Sub
  | Mult
  | SMult

(** [equ_tree] epresents the equation on matrices as a tree with nodes
    being operations, [Op_Node], and leaves being matrices [Matrix_Leaf]
    or empty [Empty_Leaf] *)
type equ_tree =
  | Matrix_Leaf of Reals.t list list
  | Op_Node of op_node
  | Empty_Leaf

and op_node = {
  op : operation;
  left : equ_tree;
  right : equ_tree;
}

(** [make_rows] populates eq.processed_rows with a list of lists, with
    each list representing the coefficients of a row. *)
val make_rows : eqs -> unit

(** [make_mat_var] takes string x representing a [matrix_var] and
    converts into this type *)
val make_mat_var : string -> matrix_var

(** [mat_eqs_fr_mut] converts from [matrix_eq_mut] to [matrix_eq] *)
val mat_eqs_fr_mut : matrix_eq_mut -> matrix_eq

(** [parse_matrix_eq] takes a [matrix_equ] type and turns into a
    [equ_tree] *)
val parse_matrix_eq : matrix_eq -> equ_tree

(** [fold_tree] calcuates a final matrix from an [equ_tree] *)
val fold_tree : equ_tree -> Reals.t list list

(** [eqrows_to_matrix] converts [eq.processed_rows] to
    [Reals.t list list] *)
val eqrows_to_matrix : eqs -> Reals.t list list

(** [parse_matrix str] converts str representing a matrix to the matrix
    of reals it represents. Requires: str containing numbers within
    square brackets, elements separated by commas, rows separated by
    semicolons. Ex: "[5, 0; 6.7, -457]" -> [Rational (5, 1); Zero];
    [Float 6.7; Rational (-457, 1)] *)
val parse_matrix : string -> Reals.t list list

(** [parse_real str] converts str representing a real value to its real.
    Requires: str represents a real *)
val parse_real : string -> Reals.t
