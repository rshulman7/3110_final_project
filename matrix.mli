(** Representation of a 2-dimensional matrix.

    This module represents 2-d matrices and includes basic matrix
    operations. *)

(** The type of elements of the matrices*)
type elt = Reals.t

(** The type of rows and columns of matrices. *)
type v = Vector.t

(** The abstract type of values representing 2-d matrices *)
type t

(** Raised when an operation is performed on a matrix that doesn't
    satisfy specific preconditions of that operation (e.g. inverting a
    nonsquare matrix). *)
exception Invalid_matrix of string

(** Raised when an operation is performed on a matrices and the
    dimensions don't satisfy the preconditions of that operation (e.g.
    multiplying and n-by-1 matrix with an n-by-1 matrix). *)
exception Dimension_mismatch of int * int

(** [make_matrix i j e] makes an [i]-by-[j] matrix with all entries
    initialized to [e].

    Requires: [n>0], [m>0]. *)
val make_matrix : int -> int -> elt -> t

(** [change_matrix_value i j e m] changes the ([i],[j])th element of [m]
    to [e]. *)
val change_matrix_value : int -> int -> elt -> t -> unit

(** [size m] returns the dimesions of [m] *)
val size : t -> int * int

(** [rows m] is a list of rows of [m], with the first row being the
    first element of the list. *)
val rows : t -> v list

(** [cols m] is a list of columns of [m], with the first column being
    the first element of the list. *)
val cols : t -> v list

(** [row_at_index m idx] is the [idx]th row of [m]. Indexing of rows
    starts at zero.

    Raises: Out_of_bounds if there is no [idx]th row in [m]. *)
val row_at_index : t -> int -> elt array

(** [col_at_index m idx] is the [idx]th column of [m]. Indexing of
    columns starts at zero.

    Raises: Out_of_bounds if there is no [idx]th column in [m]. *)
val col_at_index : t -> int -> elt array

(** [of_real_list_list ell] converts an list of lists of reals matrix,
    where inner lists are the rows of the matrix.

    Raises: Invalid_matrix if [ell] contains lists of different lengths
    or if [ell = \[\[\]\]]. *)
val of_real_list_list : Reals.t list list -> t

(** [real_list_list_of_matrix m] converts from matrix to a list of list
    of reals, where the inners lists are the rows of [m]. *)
val real_list_list_of_matrix : t -> Reals.t list list

(** [transpose m] is the transpose of matrix [m]. *)
val transpose : t -> t

(** [to_string m] is a string representation of matrix [m]. *)
val to_string : t -> string

(** [add_column v m] concatenates [v] to the right end of [m].

    Raises: Invalid_matrix if [dim v <> fst (size m)]. *)
val add_column : v -> t -> t

(** [add_row v m] concatenates [v] to the bottom of [m].

    Raises: Invalid_matrix if [dim v <> snd (size m)]. *)
val add_row : v -> t -> t

(** [eye n] creates an [n]-by-[n] identity matrix.contents

    Requires: [n > 0]. *)
val eye : int -> t

(** [diag m] is the diagonal elements of matrix [m] as a list
    $[m_{1,1}, m_{2,2},... , m_{2,2}]$.

    Requires: [m] is a square matrix. *)
val diag : t -> elt array

(** [create_diag lst] creates a [length lst]-by-[length lst] matrix with
    all entries [Zero], except the ith diagonal entry is the ith element
    of [lst] *)
val create_diag : elt list -> t

(** [rem_row idx m] removes the [idx]th column from matrix [m] according
    to zero-indexing.

    Requires: removing col [idx] doesn't make the matrix empty.

    Requires: [m] has an [idx]th column. *)
val rem_row : int -> t -> t

(** [rem_col idx m] removes the [idx]th column from matrix [m] according
    to zero-indexing.

    Requires: removing col [idx] doesn't make the matrix empty.

    Requires: [m] has an [idx]th column. *)
val rem_col : int -> t -> t

(** [of_vector_list v_lst] convertes [v_lst] into a matrix using
    elements of [v_lst] as rows.

    Raises: Invalid_matrix if elements of [v_lst] have different
    dimensions *)
val of_vector_list : v list -> t

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

(** [swap r1 r2 m] swaps rows indexed r1 and r2 of m*)
val swap : int -> int -> t -> unit

(** [matrix m1 m2] checks matrix equality rows, columns, number of
    columns and number of rows *)
val matrix_equality : t -> t -> bool

(** [rref m] performs Gaussian elimination on m *)
val rref : t -> unit

(** [det m] is the determinant of matrix [m] *)
val det : t -> elt

(** [inverse m] computes the inverse of matrix m

    requires: m is square

    raises: Invalid_matrix if not invertible *)
val inverse : t -> t
