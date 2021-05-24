(** Representation of vectors.

    This module represents vectors in real space of arbitrary dimensions
    and includes operations on vectors. *)

(** The type of elements of a vector. *)
type elt = Reals.t

(** The abstract type of values representing vectors. *)
type t

(** Raised when an operation requires vectors to be specific dimensions
    and those requirements are not met. *)
exception Dimension_mismatch

(** Raised when an element of a vector is requested but doesn't exist. *)
exception Out_of_bounds

(** [dim v] is the length/dimension of vector [v]. *)
val dim : t -> int

(** [from_reals_list elt_list] takes a list of reals and instantiates a
    vector. *)
val of_reals_list : elt list -> t

(** [to_reals_list v] is a list of reals that are the entries of [v] in
    order. *)
val to_reals_list : t -> elt list

(** [init n e] creates a vector of length [n] with all entries [e].

    Requires: [n > 0]. *)
val init : int -> elt -> t

(** [add_elt v e] adds a dimension to vector [v] and places [e] as the
    value in that dimension. *)
val add_elt : t -> elt -> t

(** [sum v1 v2] is the element-wise sum of [v1] and [v2].

    Raises: [Dimension_mismatch] if [dim v1 <> dim v2]. *)
val sum : t -> t -> t

(** [dot v1 v2] is the dot product of [v1] and [v2].

    Raises: [Dimension_mismatch] if [dim v1 <> dim v2]. *)
val dot : t -> t -> elt

(** [scalar_mult e v] multiplies each term in [v] by [e]. *)
val scalar_mult : elt -> t -> t

(** [mult_elt_wise v1 v2] multiplies each term in [v1] by each term in
    [v2]. *)
val mult_elt_wise : t -> t -> t

(** [subtract v1 v2] is the element-wise subtraction [v1] - [v2]

    Raises: [Dimension_mismatch] if [dim v1 <> dim v2] *)
val subtract : t -> t -> t

(** [lookup v idx] is the [idx]th element of [v].

    Raises: [Out_of_bounds] if [idx > (dim v - 1)] *)
val lookup : t -> int -> elt

(** [norm ~norm_type:string v] is the [norm_type] norm of [v]

    Requires: [norm_type] is a string that is one of: "1", "2", "sup"
    (with default value "2"). *)
val norm : ?norm_type:string -> t -> elt

(** [vector_equality v1 v2] checks vector equality element-wise. *)
val vector_equality : t -> t -> bool

(** [string_of_vector v] is a string representation of vector v. *)
val string_of_vector : t -> string
