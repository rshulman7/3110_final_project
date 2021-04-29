(* Implementation of the linear algebra operations *)

type elt = Reals.t

type v = Vector.t

type t = Matrix.t

exception Timeout of string

type result =
  | Result of t
  | No_result of string
  | Warning of t * string

let tol = Reals.Float 1e-10

let niter_max = 100_000

(* different types of norms!! How do we account for this? *)
let norm ?norm_type:(norm = "2") (t : t) : elt =
  failwith "Unimplemented"

let rref m v =
  let new_m = Matrix.add_column v m in
  new_m |> Matrix.rref;
  new_m

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

let mat_exp (m : t) : t = failwith "Unimplemented"

let det = Matrix.det

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

let rec q_r_alg niter m =
  if fst (Matrix.size m) = 1 then
    Matrix.real_list_list_of_matrix m |> List.hd
  else
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
      has_converged_helper m Zero <: tol
    in
    if has_converged m then Matrix.diag m |> Array.to_list
    else
      let q, r = q_and_r m in
      if niter >= niter_max then
        raise (Timeout "QR algorithm did not converge");
      q_r_alg (niter + 1) (Matrix.multiply r q)

let set idx e v =
  v.(idx) <- e;
  v

let eigenvecs_from_values m eigenvals =
  let len = m |> Matrix.size |> fst in
  List.map
    (fun eigenval ->
      let rref_m =
        m |> Matrix.(subtract (scalar_mult eigenval (eye len)))
      in
      Matrix.rref rref_m;
      Matrix.col_at_index rref_m (len - 1)
      |> set (len - 1) Reals.(Float ~-.1.)
      |> Array.to_list)
    eigenvals
  |> Matrix.of_real_list_list |> Matrix.transpose

let eig m =
  let eigenvals = q_r_alg 0 m in
  let eigenvectors = eigenvecs_from_values m eigenvals in
  (eigenvals, eigenvectors)

let check_quality_eig m =
  let eigenvals = q_r_alg 0 m in
  let len = List.length eigenvals in
  let rec helper = function
    | h :: t ->
        m
        |> Matrix.(subtract (scalar_mult h (eye len)))
        |> det |> Reals.string_of_real |> print_string;
        print_string "\n";
        let rref_m = m |> Matrix.(subtract (scalar_mult h (eye len))) in
        Matrix.rref rref_m;
        let _ = print_string ("\n" ^ Matrix.to_string rref_m ^ "\n") in
        helper t
    | [] -> ()
  in
  helper eigenvals
