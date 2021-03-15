type elt = Reals.t

type t = Vector

(* exceptions? *)

let length : t -> int = failwith "Unimplemented"

let from_string : string -> t = failwith "Unimplemented"

let add_elt : t -> elt -> t = failwith "Unimplemented"

let sum : t -> t -> t = failwith "Unimplemented"

let dot : t -> t -> float = failwith "Unimplemented"

let scalar_mult : t -> float -> t = failwith "Unimplemented"

let cross : t -> t -> t = failwith "Unimplemented"

let subtract : t -> t -> t = failwith "Unimplemented"

let lookup : t -> int -> elt = failwith "Unimplemented"

let norm : t -> float = failwith "Unimplemented"

let print : t -> unit = failwith "Unimplemented"
