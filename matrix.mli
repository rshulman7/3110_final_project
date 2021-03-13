(** what this is
	*)

type t

(* in the implementation we should have this as a list? of columns (as vector.t) , together with a list? of rows (as vector.t) *)

type vector

type index = int * int

type element = float

(* exceptions? *)

val from_string : string -> t

val add_column : t -> vector -> t

val add_row : t -> vector -> t

val sum : t -> t -> t

val scalar_mult : t -> float -> t

val multiply : t -> t -> t

val mult_elt_wise : t -> t -> t

val subtract : t -> t -> t

val lookup : t -> index -> element

val norm : t -> float (* different types of norms!!*)

val rref : t -> t

val mat_exp : t -> t
