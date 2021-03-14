(** Representation of vectors.

		This module represents vectors in real space of arbitrary dimensions, some
		operations on vectors, and creation of vectors from strings.

		AF: a vector is represented

	*)

type t

type element = float

(* exceptions? *)

val length : t -> int

val from_string : string -> t

val add_elt : t -> element -> t

val sum : t -> t -> t

val dot : t -> t -> float

val scalar_mult : t -> float -> t

val cross : t -> t -> t

val subtract : t -> t -> t

val lookup : t -> int -> element

val norm : t -> float

val print : t -> unit
