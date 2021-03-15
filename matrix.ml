(** Representation of a 2-dimensional matrix.

    This module represents 2-d matrices and includes basic matrix
    operations. It also handles creation of matrices from strings.

    AF: a matrix is represented as a length m list of vectors, which we
    think of as the columns of the matrix, together with a length n list
    of vectors, which we think of as the rows of the matrix.

    RI: list of length m exclusively contains vectors of length n, and
    the list of length n exculisively contains vectors of length m *)

type vector = Vector.t

type index = int * int

type elt = Reals.t

type t

(* in the implementation we should have this as a list? of columns (as
   vector.t) , together with a list? of rows (as vector.t) *)

(* exceptions? *)

let from_string : string -> t = failwith "Unimplemented"

let add_column : t -> vector -> t = failwith "Unimplemented"

let add_row : t -> vector -> t = failwith "Unimplemented"

let size : t -> int * int = failwith "Unimplemented"

let sum : t -> t -> t = failwith "Unimplemented"

let scalar_mult : t -> float -> t = failwith "Unimplemented"

let multiply : t -> t -> t = failwith "Unimplemented"

let mult_elt_wise : t -> t -> t = failwith "Unimplemented"

let subtract : t -> t -> t = failwith "Unimplemented"

let lookup : t -> index -> elt = failwith "Unimplemented"

(* different types of norms!! How do we account for this? *)
let norm : t -> float = failwith "Unimplemented"

let rref : t -> t = failwith "Unimplemented"

let mat_exp : t -> t = failwith "Unimplemented"

let det : t -> float = failwith "Unimplemented"

let print : t -> unit = failwith "Unimplemented"

let to_string : t -> string = failwith "Unimplemented"
