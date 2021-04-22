(** Representation of a 2-dimensional matrix.

    This module represents 2-d matrices and includes basic matrix
    operations. It also handles creation of matrices from strings. *)

type elt = Reals.t

type v = Vector.t

type t

exception Invalid_matrix of string

exception Dimension_mismatch of int * int

exception Out_of_bounds

val rows : t -> v list

val cols : t -> v list

(** [of_vector_list v_lst] converts an ordered list of vectors to a
    matrix, where the vectors will be the columns of the matrix

    raises: Dimension_Mismatch if the vectors are not all of the same
    length *)
val of_real_list_list : Reals.t list list -> t

(** [real_list_list_of_matrix m] converts from matrix to vectors *)
val real_list_list_of_matrix : t -> Reals.t list list

(** [transpose m] is the transpose of matrix [m] *)
val transpose : t -> t

(** [diag m] is the diagonal elements of matrix [m] as a list $m_{1,1},
    m_{2,2}, \ldots, m_{2,2}$. *)
val diag : t -> elt list

(** [to_string m] is the string representation of matrix [m] *)
val to_string : t -> string

(** [add_column v m] concatenates [v] to the right end of [m]

    raises: Invalid_matrix if [dim v <> fst (size m)]*)
val add_column : v -> t -> t

(** [add_row v m] concatenates [v] to the right end of [m]

    raises: Invalid_matrix if [dim v <> snd (size m)]*)
val add_row : v -> t -> t

(** [rem_col idx m] removes the [idx]th column from matrix [m]

    requires: removing col [idx] doesn't make the matrix empty raises:
    Out_of_bounds if [idx < 0 || idx > snd (size m)] *)
val rem_col : int -> t -> t

(** [rem_col idx m] removes the [idx]th row from matrix [m]

    requires: removing row [idx] doesn't make the matrix empty raises:
    Out_of_bounds if [idx < 0 || idx > fst (size m)] *)
val rem_row : int -> t -> t

(** [of_vector_list v_lst] convertes [v_lst] into a matrix of type [t]
    using elements of [v_lst] as rows

    raises: Invalid_matrix if elements of [v_lst] have different
    dimensions *)
val of_vector_list : v list -> t

(** [size m] returns the dimesions of [m] *)
val size : t -> int * int

(** [sum m1 m2] returns the matrix sum of [m1] and [m2]

    raises: Dimension_Mismatch if [size m1 <> size m2]*)
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
val lookup : t -> int * int -> elt

(** [matrix m1 m2] checks matrix equality rows, columns, number of
    columns and number of rows *)
val matrix_equality : t -> t -> bool
