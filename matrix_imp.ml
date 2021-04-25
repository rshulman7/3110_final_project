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

let of_real_list_list rll : t =
  Array.of_list rll |> Array.map Array.of_list

let real_list_list_of_matrix m : elt list list =
  Array.map Array.to_list m |> Array.to_list

let transpose m =
  let row_len, col_len = size m in
  let new_m = Array.make_matrix row_len col_len Reals.Zero in
  for i = 0 to row_len do
    for j = 0 to col_len do
      new_m.(j).(i) <- m.(i).(j)
    done
  done;
  new_m

let add_column v m =
  [ Vector.to_reals_list v ]
  |> of_real_list_list
  |> Array.append (transpose m)
  |> transpose

let add_row (v : v) (m : t) =
  [ Vector.to_reals_list v ] |> of_real_list_list |> Array.append m

let rem_row index m =
  if index = 0 then Array.sub m 1 (Array.length m)
  else if index = Array.length m then Array.sub m 0 (Array.length m - 1)
  else
    Array.sub m (index + 1) (Array.length m)
    |> Array.append (Array.sub m 0 (index - 1))

let rem_col index m = transpose m |> rem_row index |> transpose

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
