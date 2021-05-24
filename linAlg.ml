type elt = Reals.t

type v = Vector.t

type t = Matrix.t

exception Timeout of string

(** The tolerance when determining convergence of an algorithm. *)
let tol = Reals.Float 1e-10

(** Maximum number of iterations of an iterating algorithm until a
    [Timeout] is thrown. *)
let niter_max = 100_000

let rref m v =
  let new_m = Matrix.add_column v m in
  new_m |> Matrix.rref;
  new_m

let det = Matrix.det

(** [pad_or_truncate lst n padding_elt] adds [padding_elt] to [lst]
    until [lst] has length [n], or truncates [lst] so that [lst] has
    length [n]. *)
let pad_or_truncate lst n padding_elt =
  assert (n >= 0);
  let rec pad_helper acc lst n =
    match (n, lst) with
    | 0, _ -> acc
    | n, [] -> pad_helper (padding_elt :: acc) lst (n - 1)
    | n, h :: t -> pad_helper (h :: acc) t (n - 1)
  in
  List.rev (pad_helper [] lst n)

(** [orthogonalize_col col prev_cols] returns a vector of norm 1 that is
    orthogonal to all of [prev_cols], and such that the span of [col]
    and [prev_cols] is the same as the span of [prev_cols] and the value
    of this function. *)
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

(** [q_and_r m] finds the QR decomposition of [m]. *)
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

(** [has converged_helper m acc] is the sum of the [acc] and the
    absolute value of all terms below the diagonal of [m]. *)
let rec has_converged_helper m acc =
  let open Reals in
  let m' = Matrix.rem_row 0 m in
  match Matrix.cols m' with
  | [ h1; h2 ] -> acc +: Vector.norm ~norm_type:"1" h1
  | h :: t ->
      has_converged_helper (Matrix.rem_col 0 m')
        (acc +: Vector.norm ~norm_type:"1" h)
  | _ -> failwith "impossible"

(** [q_r_alg niter m] is the QR algorithm on matrix [m], meaning it is
    an upper-diagonal matrix similar to [m].

    Raises: [Timeout] if after [niter] steps the 1 norm of the terms
    below the diagonal is larger than [tol]. *)
let rec q_r_alg niter m =
  if fst (Matrix.size m) = 1 then
    Matrix.real_list_list_of_matrix m |> List.hd
  else
    let open Reals in
    let has_converged m = has_converged_helper m Zero <: tol in
    if has_converged m then Matrix.diag m |> Array.to_list
    else
      let q, r = q_and_r m in
      if niter >= niter_max then
        raise (Timeout "QR algorithm did not converge");
      q_r_alg (niter + 1) (Matrix.multiply r q)

(** [set idx e v] updates the [idx] element of [v] to [e] and returns
    [v]. *)
let set idx e v =
  v.(idx) <- e;
  v

(** [find eigenvector m len eigenval] finds the eigenvector of [m]
    associated to [eigenval].

    Requires: [m] is a square [len]-by-[len] matrix and [eigenval] is an
    approximate eigenvalue of [m]. *)
let find_eigenvector m len eigenval =
  let rref_m =
    m |> Matrix.(subtract (scalar_mult eigenval (eye len)))
  in
  Matrix.rref rref_m;
  if det rref_m <> Zero then
    Matrix.col_at_index rref_m (len - 1)
    |> set (len - 1) Reals.(Float ~-.1.)
    |> Array.to_list
  else
    let zero_vec = Vector.init len Zero in
    let cols = Matrix.cols rref_m in
    List.map
      (fun v ->
        if Vector.vector_equality zero_vec v then Reals.Rational (1, 1)
        else Reals.Zero)
      cols

(** [eigenvecs_from_values m eigenvals] is the eigenvectors of [m].

    Requires: [eigenvals] are approximate eigenvalues of [m] and [m] is
    square. *)
let eigenvecs_from_values m eigenvals =
  let len = m |> Matrix.size |> fst in
  List.map (find_eigenvector m len) eigenvals
  |> Matrix.of_real_list_list |> Matrix.transpose

let eig m =
  let eigenvals = q_r_alg 0 m in
  let eigenvectors = eigenvecs_from_values m eigenvals in
  (eigenvals, eigenvectors)
