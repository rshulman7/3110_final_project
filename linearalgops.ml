(* Implementation of the linear algebra operations *)

type elt = Reals.t

type v = Vector.t

type t = Matrix.t

type result =
  | Result of t
  | No_result of string
  | Warning of t * string

let tol = 1e-8

(* different types of norms!! How do we account for this? *)
let norm ?norm_type:(norm = "2") (t : t) : elt =
  failwith "Unimplemented"

let switch r1 r2 r =
  if Vector.vector_equality r r1 then r2
  else if Vector.vector_equality r r2 then r1
  else r

(* returns a new matrix that has rows r1 and r2 switched of m*)
let switch_rows (m : t) (r1 : v) (r2 : v) : t =
  Matrix.rows m |> List.map (switch r1 r2) |> Matrix.of_vector_list

(* divides the row by the indexed element of that row*)
let divide_row_by_indexed_elt_of_row row index =
  row
  |> Vector.scalar_mult
       (Reals.( /: ) (Reals.Float 1.) (Vector.lookup row index))

(* subtracts all rows by first_row to create a column of zeros*)
let row_subtract_in_elim row_index first_row =
  List.map (fun row ->
      Vector.scalar_mult (Vector.lookup row row_index) first_row
      |> Vector.subtract row)

let forward_elim rows =
  let row_index = ref (-1) in
  let next_row () = row_index := !row_index + 1 in
  let rec reduce_rows row_index rows =
    match rows with
    | [] -> []
    | [ h ] ->
        next_row ();
        [ divide_row_by_indexed_elt_of_row h !row_index ]
    | h :: t ->
        next_row ();
        let new_row = divide_row_by_indexed_elt_of_row h !row_index in
        new_row
        :: reduce_rows row_index
             (row_subtract_in_elim !row_index new_row t)
  in
  reduce_rows row_index rows

let check_solution_rows (m : t) =
  List.fold_left
    (fun cur_res row ->
      match cur_res with
      | No_result _ -> cur_res
      | _ -> (
          match List.rev (Vector.to_reals_list row) with
          | h :: t ->
              if
                not
                  Reals.(
                    Vector.(norm ~norm_type:"1" (of_reals_list t))
                    =: Zero)
              then cur_res
              else if Reals.(h =: Zero) then
                Warning (m, "infinitely many solutions")
              else No_result "there are no solutions"
          | [] -> assert false))
    (Result m) (Matrix.rows m)

let rref (m : t) (v : v) =
  let rows = Matrix.add_column v m |> Matrix.rows in
  forward_elim rows |> Matrix.of_vector_list

(*|> check_solution_rows*)
let backward_solve rows =
  let row_index = ref (List.length rows) in
  let next_row () = row_index := !row_index - 1 in
  let rec reduce_rows row_index rows =
    match rows with
    | [] -> []
    | [ h ] ->
        next_row ();
        [ divide_row_by_indexed_elt_of_row h !row_index ]
    | h :: t ->
        next_row ();
        let new_row = divide_row_by_indexed_elt_of_row h !row_index in
        new_row
        :: reduce_rows row_index
             (row_subtract_in_elim !row_index new_row t)
  in
  reduce_rows row_index rows

let rref (m : t) (v : v) : t =
  let rows = Matrix.add_column v m |> Matrix.rows in
  forward_elim rows |> List.rev |> backward_solve |> List.rev
  |> Matrix.of_vector_list

let mat_exp (m : t) : t = failwith "Unimplemented"

let rec det m =
  assert (m |> Matrix.size |> fst = (m |> Matrix.size |> snd));
  if Matrix.size m = (1, 1) then Matrix.lookup m (0, 0)
  else
    match Matrix.rows m with
    | h :: t -> List.fold_left Reals.( +: ) Reals.Zero (iterate_det h t)
    | [] -> assert false

and iterate_det h t =
  List.mapi
    (list_map_det (Matrix.of_vector_list t))
    (Vector.to_reals_list h)

and list_map_det m i a =
  Reals.( *: )
    (if i mod 2 = 0 then a else Reals.(~-:a))
    (det Matrix.(m |> rem_col i))

(** my shitty implementation of rref *)

(* let rec my_rref acc (rows, cols, nr, nc) cur_r = let open Reals in
   match (find_working_row cur_r cols, cols) with | _, [] -> acc | None,
   h :: t -> my_rref (h :: acc) (rows, t, nr, nc) cur_r | Some n, _ ->
   swap_if_necessary rows cur_r n

   and find_working_row start_r cols = match cols with | h :: t ->
   Vector.to_reals_list h |> find_nonzero_above start_r | [] -> None

   and find_nonzero_above n lst = match lst with | [] -> None | h :: t
   -> if n > 0 || h <> Zero then find_nonzero_above (n - 1) t else Some
   n

   and swap_if_necessary rows cur_r target = if target = cur_r then rows
   else rows

   and one_step_rref r = false

   and find_nonzero r col = false

   and scale r c m = false

   and swap = false

   and subtract = false

   let my_rref_caller (m : Matrix.t) = let a = Matrix.(rows m, cols m,
   fst (size m), snd (size m)) in my_rref [] a *)

let rec pad_or_truncate lst n padding_elt =
  assert (n >= 0);
  let rec pad_helper acc lst n =
    match (n, lst) with
    | 0, _ -> acc
    | n, [] -> pad_helper (padding_elt :: acc) lst (n - 1)
    | n, h :: t -> pad_helper (h :: acc) t (n - 1)
  in
  List.rev (pad_helper [] lst n)

let orthogonalize_col col prev_cols =
  let open Reals in
  let open Vector in
  let projections = List.map (dot col) prev_cols in
  let orthogonal_col =
    subtract col
      (List.fold_left2
         (fun acc a b -> sum acc (scalar_mult a b))
         (scalar_mult Zero col) projections prev_cols)
  in
  let orthonormal_col =
    scalar_mult (Float 1. /: norm orthogonal_col) orthogonal_col
  in
  let diag_col =
    pad_or_truncate
      (List.rev (dot col orthonormal_col :: projections))
      (dim col) Zero
  in
  (orthonormal_col, diag_col)

let q_and_r m =
  let rec q_and_r_helper completed_cols completed_diag cols_2_go =
    match cols_2_go with
    | [] -> (completed_cols, completed_diag)
    | h :: t ->
        let c_new, d_new = orthogonalize_col h completed_cols in
        q_and_r_helper
          (c_new :: completed_cols)
          (d_new :: completed_diag)
          t
  in
  let q, r = q_and_r_helper [] [] (Matrix.cols m) in
  Matrix.
    ( q |> List.rev |> of_vector_list |> transpose,
      r |> List.rev |> of_real_list_list |> transpose )

let rec q_r_alg m =
  let open Reals in
  let has_converged m =
    let rec has_converged_helper m acc =
      let m' = Matrix.rem_row 0 m in
      match Matrix.cols m' with
      | [ h1; h2 ] -> acc +: Vector.norm ~norm_type:"1" h1
      | h :: t ->
          has_converged_helper (Matrix.rem_col 0 m')
            (acc +: Vector.norm ~norm_type:"1" h)
      | _ -> failwith "impossible"
    in
    has_converged_helper m Zero <: Float tol
  in
  if has_converged m then Matrix.diag m
  else
    let q, r = q_and_r m in
    q_r_alg (Matrix.multiply r q)

let eig = q_r_alg
