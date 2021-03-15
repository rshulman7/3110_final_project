(** Representation of a 2-dimensional matrix.

    This module represents 2-d matrices and includes basic matrix
    operations. It also handles creation of matrices from strings. *)

type elt = Reals.t

type v = Vector.t

type index = int * int

type t

exception Dimension_Mismatch

exception Out_of_Bounds

(** [of_vector_list v_lst] converts an ordered list of vectors to a
    matrix, where the vectors will be the columns of the matrix

    raises: Dimension_Mismatch if the vectors are not all of the same
    length *)
val of_vector_list : v list -> t

(** [to_vector_list m] converts matrix [m] to an ordered list of
    vectors, where the vectors are the columns of [m] *)
val to_vector_list : t -> v list

(** [to_string m] is the string representation of matrix [m] *)
val to_string : t -> string

(* the following two can be hidden *)
(* val add_column : v -> t -> t *)
(* val add_row : v -> t -> t *)

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
val lookup : t -> index -> elt

(* MAYBE ALL OF THE FOLLOWING SHOULD BE IN A NEW MODULE *)

(** [norm ?norm_type m] is the [?norm_type] norm of [m]. The default
    [?norm_type] is "2", which represents the 2-norm.

    requires: [?norm_type] is one of "2", "1", "op" *)
val norm : ?norm_type:string -> t -> float

(** [rref m] is [m] row reduced into echelon form. Specifically, all
    pivots are 1. *)
val rref : t -> t

(** [mat_exp m] is the matrix exponential of matrix [m] *)
val mat_exp : t -> t

(** [det m] is the determinant of matrix [m] *)
val det : t -> elt
