(** AF: a matrix is represented as a length m ordered list of vectors
    each of length n, which we think of as the columns of the matrix,
    together with a length n ordered list of vectors each of length m,
    which we think of as the rows of the matrix.

    RI: list of length m exclusively contains vectors of length n, and
    the list of length n exculisively contains vectors of length m *)

type elt = Reals.t

type v = Vector.t

type index = int * int

(* rows, columns, number of rows, number of columns*)
type t = v list * v list * int * int

exception Invalid_matrix of string

exception Dimension_mismatch of int * int

exception Out_of_bounds

let size ((_, _, a, b) : t) = (a, b)

let cols ((c, _, _, _) : t) = c

let rows ((_, r, _, _) : t) = r

let rep_ok t =
  let open Vector in
  let n = t |> size |> fst and m = t |> size |> snd in
  if
    t |> rows |> List.fold_left (fun a b -> a && dim b == m) true
    && t |> cols |> List.fold_left (fun a b -> a && dim b == n) true
  then t
  else raise (Invalid_matrix "failed RI check")

(** [transverse ll] converts a list of lists to a list of lists by
    combining the ith element of each list into a new list, in order.

    requires: all lists in [ll] are of the same length *)
let transverse (ll : 'a list list) =
  let rec helper rows cols =
    match (cols, rows) with
    | [], h :: t -> h |> List.map (fun a -> [ a ]) |> helper t
    | _, h :: t -> h |> List.map2 (fun a b -> b :: a) cols |> helper t
    | _, [] -> cols
  in
  helper (List.rev ll) []

let rlst_of_vlst : Vector.t list -> Reals.t list list =
  List.map (fun a -> Vector.to_reals_list a)

let vlst_of_rlst : Reals.t list list -> Vector.t list =
  List.map (fun a -> Vector.of_reals_list a)

let of_real_list_list rll : t =
  match rll with
  | [] -> raise (Invalid_matrix "tried to create an empty matrix")
  | h :: t ->
      if List.(fold_left (fun a b -> a && length b == length h)) true t
      then
        ( vlst_of_rlst rll,
          vlst_of_rlst (transverse rll),
          List.length rll,
          List.length h )
      else raise (Invalid_matrix "Rows are not all the same length!")

let of_vector_list (v_lst : v list) =
  v_lst |> rlst_of_vlst |> of_real_list_list

let to_vector_list ((a, _, _, _) : t) = a

let real_list_list_of_matrix m =
  List.map (fun a -> Vector.to_reals_list a) (to_vector_list m)

let transpose ((rows, cols, nr, nc) : t) : t = (cols, rows, nc, nr)

let add_row vec m = of_vector_list (to_vector_list m @ [ vec ])

let add_column m vec = m |> transpose |> add_row vec |> transpose

let size ((_, _, nr, nc) : t) = (nr, nc)

let sum (m1 : t) (m2 : t) =
  match (m1, m2) with
  | (r1, c1, nr1, nc1), (r2, c2, nr2, nc2) ->
      if nr1 <> nr2 then raise (Dimension_mismatch (nr1, nr2))
      else if nc1 <> nc2 then raise (Dimension_mismatch (nc1, nc2))
      else List.map2 (fun a b -> Vector.sum a b) r1 r2

let scalar_mult (elt : elt) (t : t) : t = failwith "Unimplemented"

let multiply (t : t) (t : t) : t = failwith "Unimplemented"

let mult_elt_wise (t : t) (t : t) : t = failwith "Unimplemented"

let subtract (t : t) (t : t) : t = failwith "Unimplemented"

let lookup (t : t) (index : index) : elt = failwith "Unimplemented"

(* different types of norms!! How do we account for this? *)
let norm ?norm_type:(norm = "2") (t : t) : elt =
  failwith "Unimplemented"

let rref (t : t) : t = failwith "Unimplemented"

let mat_exp (t : t) : t = failwith "Unimplemented"

let det (t : t) : elt = failwith "Unimplemented"

let to_string (t : t) : string = failwith "Unimplemented"
