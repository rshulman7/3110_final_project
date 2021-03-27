(** Representation of a 2-dimensional matrix.

    This module represents 2-d matrices and includes basic matrix
    operations. It also handles creation of matrices from strings. *)

type elt = Reals.t

type v = Vector.t

type index = int * int

type t

exception Invalid_matrix of string

exception Dimension_mismatch of int * int

exception Out_of_bounds

(** [of_vector_list v_lst] converts an ordered list of vectors to a
    matrix, where the vectors will be the columns of the matrix

    raises: Dimension_Mismatch if the vectors are not all of the same
    length *)
val of_real_list_list : Reals.t list list -> t

(** [real_list_list_of_matrix m] converts from matrix to vectors *)
val real_list_list_of_matrix : t -> Reals.t list list

(** [to_vector_list m] converts matrix [m] to an ordered list of
    vectors, where the vectors are the columns of [m] *)

(* val to_real_list_list : t -> elt list list *)

(** [to_string m] is the string representation of matrix [m] *)
val to_string : t -> string

(* the following two can be hidden *)
(* I think we need to make rows, cols, add_column and add_row visible so
   rref in linearalgoops can use them- SY*)
val add_column : v -> t -> t

val add_row : v -> t -> t

val rows : t -> v list

val cols : t -> v list

(** [size m] returns the dimesions of [m] *)
val size : t -> int * int

(** [sum m1 m2] returns the matrix sum of [m1] and [m2]

    raises: Dimension_Mismatch if [size m1 <> size m2]*)

(* this needs to return a type t. i have changed it to Vector.t list so
   it compiles with the current implementation RES *)
val sum : t -> t -> t

(** [scalar_mult e m] is the matrix [m] with each entry multiplied by
    [e] *)
val scalar_mult : elt -> t -> t

(** [multiply m1 m2] is the matrix multiplication of [m1] and [m2]

    raises: Dimension_Mismatch if [snd (size m1) <> fst (size m2)]*)
val multiply : t -> t -> t

(** [mult_elt_wise m1 m2] is the element-wise multiplication of matrices
    [m1] and [m2]

    raises: Dimension_Mismatch if [size m1 <> size m2]*)
val mult_elt_wise : t -> t -> t

(** [subtract m1 m2] subtracts matrix [m2] from [m1]

    raises: Dimension_Mismatch if [size m1 <> size m2] *)
val subtract : t -> t -> t

(** [lookup m idx] finds the [idx] element of matrix [m]

    raises: Out_of_Bounds if [idx] is not a valid index for [m] *)
val lookup : t -> index -> elt

(** [matrix m1 m2] checks matrix equality rows, columns, number of
    columns and number of rows *)
val matrix_equality : t -> t -> bool
