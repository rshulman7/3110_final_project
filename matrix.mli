(** Representation of a 2-dimensional matrix.

		This module represents 2-d matrices and includes basic matrix operations. It
		also handles creation of matrices from strings.

		AF: a matrix is represented as a length m list of vectors, which we think of
		as the columns of the matrix, together with a length n list of vectors, 
		which we think of as the rows of the matrix.

		RI: list of length m exclusively contains vectors of length n, and the list 
		of length n exculisively contains vectors of length m
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

val size : t -> int * int

val sum : t -> t -> t

val scalar_mult : t -> float -> t

val multiply : t -> t -> t

val mult_elt_wise : t -> t -> t

val subtract : t -> t -> t

val lookup : t -> index -> element

(* different types of norms!! How do we account for this? *)
val norm : t -> float

val rref : t -> t

val mat_exp : t -> t

val det : t -> float

val print : t -> unit

(* QUESTION: do we want to support only for floats, or do we want to overload 
		all our operations to support fractions/integers as well? *)
