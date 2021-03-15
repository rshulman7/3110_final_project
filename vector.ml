(** AF: a vector is represented as a [(Real.t list * int)] pair where
    [int] is the dimension of the vector and the [Real.t list] is the
    vector in order

    RI: length of [Real.t list] is the same as [int]. *)

type elt = Reals.t

type t = Vector of elt list * int

exception Dimension_Mismatch

let dim : t -> int = failwith "Unimplemented"

let add_elt : t -> elt -> t = failwith "Unimplemented"

let of_reals_list = failwith "Unimplemented"

let to_reals_list = failwith "Unimplemented"

let sum : t -> t -> t = failwith "Unimplemented"

let dot : t -> t -> elt = failwith "Unimplemented"

let scalar_mult : t -> elt -> t = failwith "Unimplemented"

let cross : t -> t -> t = failwith "Unimplemented"

let subtract : t -> t -> t = failwith "Unimplemented"

let lookup : t -> int -> elt = failwith "Unimplemented"

let norm = failwith "Unimplemented"

let to_string : t -> string = failwith "Unimplemented"
