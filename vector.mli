(** Representation of vectors.

    This module represents vectors in real space of arbitrary
    dimensions, some operations on vectors, and creation of vectors from
    strings. *)

type elt = Reals.t

type t

exception Dimension_Mismatch

(** [dim v] is the length/dimension of vector [v] *)
val dim : t -> int

(** [add_elt v e] adds a dimension to vector [v] and places [e] as the
    value in that dimension *)
val add_elt : t -> elt -> t

(** [from_reals_list elt_list] takes a list of reals and instantiates a
    vector *)

(** [to_reals_list v] is a list of reals that are entries of [v] in
    order*)
val to_reals_list : t -> elt list

(** [sum v1 v2] is the element-wise sum of [v1] and [v2]

    raises: Dimension_Mismatch if [dim v1 <> dim v2]*)
val sum : t -> t -> t

(** [dot v1 v2] is the standard dot product of [v1] and [v2]

    raises: Dimension_Mismatch if [dim v1 <> dim v2] *)
val dot : t -> t -> elt

(** [scalar_mult e v] multiplies each term in [v] by [e] *)
val scalar_mult : elt -> t -> t

(** [cross v1 v2] is the 3-d cross product of [v1] and [v2]

    requires: [v1] and [v2] are both 3-dimensional *)
val cross : t -> t -> t

(** [subtract v1 v2] is the element-wise subtraction [v1]-[v2]

    raises: Dimension_Mismatch if [dim v1 <> dim v2] *)
val subtract : t -> t -> t

val lookup : t -> int -> elt

(** [norm ?norm_type:string v1] is the [norm_type] norm of [v1]. Here
    [norm_type] is a string that is one of: "fro", "sup" *)
val norm : ?norm_type:string -> t -> elt

(** [to_string v] is the string representation of vector v*)
val to_string : t -> string
