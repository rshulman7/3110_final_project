(** what this is
	*)

type t

type element = float

(* exceptions? *)

val from_string : string -> t

val add_elt : t -> element -> t

val sum : t -> t -> t

val dot : t -> t -> float

val scalar_mult : t -> float -> t

val cross : t -> t -> t

val subtract : t -> t -> t

val lookup : t -> int -> element

val norm : t -> float
