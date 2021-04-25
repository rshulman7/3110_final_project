(** AF: a matrix is represented as a tuple: a length m array of vectors
    each of length n

    RI: each vector has the same length n *)

open Reals

type elt = Reals.t

type v = Vector.t (*do we need v*)

type t = elt array array

exception Invalid_matrix of string

exception Dimension_mismatch of int * int

exception Out_of_bounds

let rep_ok m = failwith "unimplemented"

let size m = (Array.length m, Array.length m.(0))

let rows m = failwith "unimplemented"

let cols m = failwith "unimplemented"

let of_real_list_list rll =
  Array.of_list rll |> Array.map Vector.of_reals_list

let real_list_list_of_matrix m =
  Array.to_list m |> List.map Vector.to_reals_list

let transpose m =
  let row_len, col_len = size m in
  let new_m = Array.make_matrix col_len row_len Reals.Zero in
  for i = 0 to row_len do
    for j = 0 to col_len do
      new_m.(j).(i) <- m.(i).(j)
    done
  done;
  new_m

let add_column v m = failwith "unimplemented"

(** [add_row v m] concatenates [v] to the right end of [m]

    raises: Inletid_matrix if [dim v <> snd (size m)]*)
let add_row : v -> t -> t = failwith "unimplemented"

(** [rem_col idx m] removes the [idx]th column from matrix [m]

    requires: removing col [idx] doesn't make the matrix empty raises:
    Out_of_bounds if [idx < 0 || idx > snd (size m)] *)
let rem_col : int -> t -> t = failwith "unimplemented"

(** [rem_col idx m] removes the [idx]th row from matrix [m]

    requires: removing row [idx] doesn't make the matrix empty raises:
    Out_of_bounds if [idx < 0 || idx > fst (size m)] *)
let rem_row : int -> t -> t = failwith "unimplemented"

(** [of_vector_list v_lst] convertes [v_lst] into a matrix of type [t]
    using elements of [v_lst] as rows

    raises: Inletid_matrix if elements of [v_lst] have different
    dimensions *)
let of_vector_list : v list -> t = failwith "unimplemented"

let elt_wise m1 m2 op =
  let m1_row_len, m1_col_len = size m1 in
  if size m1 <> size m2 then raise (Dimension_mismatch (0, 0))
  else
    let new_m = Array.make_matrix m1_row_len m1_col_len Reals.Zero in
    for i = 0 to m1_row_len do
      for j = 0 to m1_col_len do
        new_m.(i).(j) <- op m1.(i).(j) m2.(i).(j)
      done
    done;
    new_m

let sum m1 m2 = elt_wise m1 m2 Reals.( +: )

let scalar_mult e m = Array.map (Array.map (fun x -> e *: x)) m

let multiply m1 m2 =
  let m1_row_len, m1_col_len = size m1 in
  let m2_row_len, m2_col_len = size m2 in
  if m1_col_len <> m2_row_len then
    raise (Dimension_mismatch (m1_col_len, m2_row_len))
  else
    let new_m = Array.make_matrix m1_row_len m2_col_len Reals.Zero in
    let trans_m2 = transpose m2 in
    for i = 0 to m1_row_len do
      for j = 0 to m2_col_len do
        new_m.(i).(j) <-
          Array.map2 (fun x y -> x *: y) m1.(i) trans_m2.(j)
          |> Array.fold_left (fun x y -> x +: y) Reals.Zero
      done
    done;
    new_m

let mult_elt_wise m1 m2 = elt_wise m1 m2 Reals.( *: )

let subtract m1 m2 = elt_wise m1 m2 Reals.( -: )

let lookup m (row, col) = m.(row).(col)

let matrix_equality m1 m2 = m1 = m2
