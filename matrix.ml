(** AF: a matrix is represented as a length m ordered list of vectors
    each of length n, which we think of as the columns of the matrix,
    together with a length n ordered list of vectors each of length m,
    which we think of as the rows of the matrix.

    RI: list of length m exclusively contains vectors of length n, and
    the list of length n exculisively contains vectors of length m *)

type elt = Reals.t

type v = Vector.t

type index = int * int

type t = v list * v list * int * int

exception Dimension_Mismatch of int * int

exception Out_of_Bounds

let size ((_, _, a, b) : t) = (a, b)

let cols ((c, _, _, _) : t) = c

let rows ((_, r, _, _) : t) = r

let rep_ok t =
  let open Vector in
  let n = t |> size |> fst and m = t |> size |> snd in
  t |> rows |> List.fold_left (fun a b -> true && dim b == m) true
  && t |> cols |> List.fold_left (fun a b -> true && dim b == n) true

let of_vector_list : v list -> t = failwith "Unimplemented"

let to_vector_list = failwith "Unimplemented"

let add_column : t -> v -> t = failwith "Unimplemented"

let add_row : t -> v -> t = failwith "Unimplemented"

let size : t -> int * int = failwith "Unimplemented"

let sum : t -> t -> t = failwith "Unimplemented"

let scalar_mult : elt -> t -> t = failwith "Unimplemented"

let multiply : t -> t -> t = failwith "Unimplemented"

let mult_elt_wise : t -> t -> t = failwith "Unimplemented"

let subtract : t -> t -> t = failwith "Unimplemented"

let lookup : t -> index -> elt = failwith "Unimplemented"

(* different types of norms!! How do we account for this? *)
let norm : ?norm_type:string -> t -> elt = failwith "Unimplemented"

let rref : t -> t = failwith "Unimplemented"

let mat_exp : t -> t = failwith "Unimplemented"

let det : t -> elt = failwith "Unimplemented"

let to_string : t -> string = failwith "Unimplemented"
