(** AF: a matrix is represented as a tuple: a length m array of vectors
    each of length n

    RI: each vector has the same length n *)

open Reals

type elt = Reals.t

type v = Vector.t

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

let row_at_index m ind = m.(ind)

let col_at_index m ind = (transpose m).(ind)

let square m =
  let rows, cols = size m in
  rows = cols

let diag m =
  if square m then (
    let rows = Array.length m in
    let diags = Array.make rows Reals.Zero in
    for i = 0 to rows do
      diags.(i) <- m.(i).(i)
    done;
    diags)
  else raise (Failure "not square")

let string_row_to_string row = Array.fold_left (fun a b -> a ^ b) "" row

let to_string m =
  Array.map (Array.map (fun x -> Reals.string_of_real x)) m
  |> Array.fold_left (fun x y -> string_row_to_string y ^ x) ""

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
  let m1_row_len, m1_col_len = size m1
  and m2_row_len, m2_col_len = size m2 in
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

(*(** AF: a matrix is represented as a length m ordered list of vectors
  each of length n, which we think of as the columns of the matrix,
  together with a length n ordered list of vectors each of length m,
  which we think of as the rows of the matrix.

  RI: list of length m exclusively contains vectors of length n, and the
  list of length n exculisively contains vectors of length m *)

  type elt = Reals.t

  type v = Vector.t

  (* rows, columns, number of rows, number of columns*) type t = v list
  * v list * int * int

  exception Invalid_matrix of string

  exception Dimension_mismatch of int * int

  exception Out_of_bounds

  let size ((_, _, a, b) : t) = (a, b)

  let rows ((r, _, _, _) : t) = r

  let cols ((_, c, _, _) : t) = c

  let rep_ok t = let open Vector in let n, m = size t in if t |> rows |>
  List.fold_left (fun a b -> a && dim b == m) true && t |> cols |>
  List.fold_left (fun a b -> a && dim b == n) true then t else raise
  (Invalid_matrix "failed RI check")

  (** [transverse ll] converts a list of lists to a list of lists by
  combining the ith element of each list into a new list, in order.

  requires: all lists in [ll] are of the same length *) let transverse
  (ll : 'a list list) = let rec helper rows cols = match (cols, rows)
  with | [], h :: t -> h |> List.map (fun a -> [ a ]) |> helper t | _, h
  :: t -> h |> List.map2 (fun a b -> b :: a) cols |> helper t | _, [] ->
  cols in helper (List.rev ll) []

  let rlst_of_vlst : Vector.t list -> Reals.t list list = List.map (fun
  a -> Vector.to_reals_list a)

  let vlst_of_rlst : Reals.t list list -> Vector.t list = List.map (fun
  a -> Vector.of_reals_list a)

  let of_real_list_list rll : t = match rll with | [] -> raise
  (Invalid_matrix "tried to create an empty matrix") | h :: t -> if
  List.(fold_left (fun a b -> a && length b == length h)) true t then (
  vlst_of_rlst rll, vlst_of_rlst (transverse rll), List.length rll,
  List.length h ) else raise (Invalid_matrix "Rows are not all the same
  length!")

  let of_vector_list (v_lst : v list) = v_lst |> rlst_of_vlst |>
  of_real_list_list

  let real_list_list_of_matrix m = rlst_of_vlst (rows m)

  let transpose ((rows, cols, nr, nc) : t) : t = (cols, rows, nc, nr)

  let add_row vec m = of_vector_list (rows m @ [ vec ])

  let add_column vec m = m |> transpose |> add_row vec |> transpose

  let rem_idx_from_list idx lst bound = if idx > bound - 1 || idx < 0
  then raise Out_of_bounds else let rec helper idx = function | h :: t
  -> if idx > 0 then h :: helper (idx - 1) t else t | [] -> failwith
  "idx out of bounds" in helper idx lst

  let rem_row idx ((rows, _, nr, _) : t) = rem_idx_from_list idx rows nr
  |> of_vector_list

  let rem_col idx m = m |> transpose |> rem_row idx |> transpose

  let same_dims ((_, _, nr1, nc1) : t) ((_, _, nr2, nc2) : t) = if nr1
  <> nr2 then raise (Dimension_mismatch (nr1, nr2)) else if nc1 <> nc2
  then raise (Dimension_mismatch (nc1, nc2))

  let helper_elt_wise (op : Vector.t -> Vector.t -> Vector.t) m1 m2 : t
  = same_dims m1 m2; ( List.map2 (fun a b -> op a b) (rows m1) (rows
  m2), List.map2 (fun a b -> op a b) (cols m1) (cols m2), fst (size m1),
  fst (size m2) )

  let sum = helper_elt_wise Vector.sum

  let scalar_mult (e : elt) (m : t) : t = let r, c, nr, nc = m in (
  List.map (fun r -> Vector.scalar_mult e r) r, List.map (fun c ->
  Vector.scalar_mult e c) c, nr, nc )

  (* let multiply ((r1, c1, nr1, nc1) : t) ((r2, c2, nr2, nc2) : t) : t
  = let rec row_mult_all_cols row cols = match cols with | [] -> [] | h
  :: t -> [ Vector.dot row h ] @ row_mult_all_cols row t in let rec
  row_mult_get_full_row_lst rows cols = match rows with | [] -> [] | h
  :: t -> [ row_mult_all_cols h cols ] @ row_mult_get_full_row_lst t
  cols in if nc1 <> nr2 then raise (Dimension_mismatch (nc1, nc2)) else
  row_mult_get_full_row_lst r1 c2 |> of_real_list_list *)

  let multiply ((rows, _, _, nc) : t) ((_, cols, nr, _) : t) : t = if nc
  <> nr then raise (Dimension_mismatch (nc, nr)) else let row_mult row =
  List.map (Vector.dot row) cols in List.map row_mult rows |>
  of_real_list_list

  let mult_elt_wise = helper_elt_wise Vector.mult_elt_wise

  let subtract = helper_elt_wise Vector.subtract

  (* do we start indexing at (0,0) or the natural (1,1)?*) let lookup
  ((r, c, nr, nc) : t) (a, b) : elt = Vector.lookup (List.nth r a) b

  let rec diag (((_, _, nr, nc) : t) as m) = match size m with | 1, _ |
  _, 1 -> [ lookup m (0, 0) ] | n1, n2 -> lookup m (0, 0) :: diag (m |>
  rem_row 0 |> rem_col 0)

  (* let matrix_equality ((r1, c1, nr1, nc1) : t) ((r2, c2, nr2, nc2) :
  t) : bool = let rec check_vector_lst v1_lst v2_lst = match (v1_lst,
  v2_lst) with | [], [] -> true | [], _ -> false | _, [] -> false | h1
  :: t1, h2 :: t2 -> Vector.vector_equality h1 h2 && check_vector_lst t1
  t2 in check_vector_lst r1 r2 && check_vector_lst c1 c2 && nr1 = nr2 &&
  nc1 = nc2 *)

  let matrix_equality ((r1, c1, nr1, nc1) as m1 : t) ((r2, c2, nr2, nc2)
  as m2 : t) = same_dims m1 m2; List.for_all2 Vector.vector_equality r1
  r2

  let to_string ((r1, c1, nr1, nc1) : t) : string = let rec printer
  v_lst = match v_lst with | [] -> "" | h :: t ->
  Vector.string_of_vector h ^ "; \n" ^ printer t in printer r1 *)
