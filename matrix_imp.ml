(** AF: a matrix is represented as a length m array of vectors each of
    length n, where each the mth vector is the mth row of the matrix.

    RI: each vector has the same length n *)

type elt = Reals.t

type v = Vector.t

type t = v array

exception Invalid_matrix of string

exception Dimension_mismatch of int * int

exception Out_of_bounds

let rep_ok m = failwith "unimplemented"

let size m = (Array.length m, m.(0) |> Vector.dim)

let rows m = Array.to_list m

let cols m = failwith "unimplemented"

let of_real_list_list rll =
  Array.of_list rll |> Array.map Vector.of_reals_list

let real_list_list_of_matrix m =
  Array.to_list m |> List.map Vector.to_reals_list

let transpose m = cols m |> Array.of_list
