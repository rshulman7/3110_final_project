(* Implementation of the linear algebra operations *)

type elt = Reals.t

type v = Vector.t

type t = Matrix.t

(* different types of norms!! How do we account for this? *)
let norm ?norm_type:(norm = "2") (t : t) : elt =
  failwith "Unimplemented"

let switch_rows (m : t) : t = failwith "Unimplemented"

let rref (m : t) (v : v) : v = failwith "Unimplemented"

let mat_exp (t : t) : t = failwith "Unimplemented"

let det (t : t) : elt = failwith "Unimplemented"
