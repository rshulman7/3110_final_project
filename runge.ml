(** Implementation of the Runge–Kutta method for solving first-order
    linear ODEs. *)
type elt = Reals.t

type v = Vector.t

type t = Matrix.t

exception Invalid_end_time

exception Invalid_step_size

open Reals
open Plot
open Format

let apply_coefs_k1 coefs_for_row prev_values num_of_elts =
  let adder = ref Reals.Zero in
  for rows = 0 to num_of_elts do
    if rows <> num_of_elts then
      match coefs_for_row.(rows) with
      | Sin ->
          adder :=
            !adder
            +: Float (sin (Reals.float_of_real prev_values.(rows)))
      | Cos ->
          adder :=
            !adder
            +: Float (cos (Reals.float_of_real prev_values.(rows)))
      | Exp -> adder := !adder +: exp prev_values.(rows)
      | rat_or_fl -> adder := !adder +: (rat_or_fl *: prev_values.(rows))
    else adder := !adder +: coefs_for_row.(rows)
  done;
  !adder

let apply_coefs_k2_3_4
    coefs_for_row
    prev_values
    num_of_elts
    kth
    step_size
    multiplier =
  let adder = ref Reals.Zero in
  for rows = 0 to num_of_elts do
    if rows <> num_of_elts then
      match coefs_for_row.(rows) with
      | Sin ->
          adder :=
            !adder
            +: Float
                 (sin
                    (Reals.float_of_real
                       (prev_values.(rows)
                       +: (step_size *: kth *: multiplier))))
      | Cos ->
          adder :=
            !adder
            +: Float
                 (cos
                    (Reals.float_of_real
                       (prev_values.(rows)
                       +: (step_size *: kth *: multiplier))))
      | Exp ->
          adder :=
            !adder
            +: exp
                 (prev_values.(rows) +: (step_size *: kth *: multiplier))
      | rat_or_fl ->
          adder :=
            !adder
            +: rat_or_fl
               *: (prev_values.(rows)
                  +: (step_size *: kth *: multiplier))
    else adder := !adder +: coefs_for_row.(rows)
  done;
  !adder

let rk_computation coef_matrix soln_matrix num_rows col step_size =
  let half = Reals.Rational (1, 2) in
  let sixth = Reals.Rational (1, 6) in
  let prev_time = Matrix.lookup soln_matrix (0, col - 1) in
  Matrix.change_matrix_value 0 col (prev_time +: step_size) soln_matrix;
  let prev_values_and_time =
    Matrix.col_at_index soln_matrix (col - 1)
  in
  let prev_values =
    Array.sub prev_values_and_time 1
      (Array.length prev_values_and_time - 1)
  in
  for rows = 1 to num_rows do
    let prev_value = prev_values.(rows - 1)
    and coefs_for_row = Matrix.row_at_index coef_matrix (rows - 1) in
    let k1 = apply_coefs_k1 coefs_for_row prev_values num_rows in
    let k2 =
      apply_coefs_k2_3_4 coefs_for_row prev_values num_rows k1 step_size
        half
    in
    let k3 =
      apply_coefs_k2_3_4 coefs_for_row prev_values num_rows k2 step_size
        half
    in
    let k4 =
      apply_coefs_k2_3_4 coefs_for_row prev_values num_rows k3 step_size
        (Reals.Float 1.)
    in
    Matrix.change_matrix_value rows col
      (prev_value
      +: sixth *: step_size
         *: (k1 +: (Reals.Float 2.0 *: k2) +: (Reals.Float 2.0 *: k3)
           +: k4))
      soln_matrix
  done;
  ()

(* let rk_computations soln_matrix col step_size coef constant = let
   half = Reals.Rational (1, 2) in let sixth = Reals.Rational (1, 6) in
   let prev_time = Matrix.lookup soln_matrix (0, col - 1) in let
   prev_value = Matrix.lookup soln_matrix (1, col - 1) in let k1 = (coef
   *: prev_value) +: constant in let k2 = (coef *: (prev_value +:
   (step_size *: k1 *: half))) +: constant in let k3 = (coef *:
   (prev_value +: (step_size *: k2 *: half))) +: constant in let k4 =
   (coef *: (prev_value +: (step_size *: k3))) +: constant in
   Matrix.change_matrix_value 0 col (prev_time +: step_size)
   soln_matrix; Matrix.change_matrix_value 1 col (prev_value +: sixth *:
   step_size *: (k1 +: (Reals.Float 2.0 *: k2) +: (Reals.Float 2.0 *:
   k3) +: k4)) soln_matrix *)
let rk plot m init_cond end_time step_size =
  if step_size <: Reals.Zero then raise Invalid_step_size
  else if end_time <: Reals.Zero then raise Invalid_end_time
  else
    let num_cols =
      end_time /: step_size |> float_of_real |> Float.ceil
      |> int_of_float
    and num_rows_of_soln_matrix = fst (Matrix.size m) + 1 in
    let soln_matrix =
      Matrix.make_matrix num_rows_of_soln_matrix (num_cols + 1)
        Reals.Zero
    in
    for row = 1 to num_rows_of_soln_matrix - 1 do
      Matrix.change_matrix_value row 0
        (Vector.lookup init_cond (row - 1))
        soln_matrix
    done;
    for col = 1 to num_cols do
      rk_computation m soln_matrix
        (num_rows_of_soln_matrix - 1)
        col step_size
    done;
    if plot && Vector.dim init_cond = 1 then make_plot soln_matrix;
    if plot && Vector.dim init_cond = 2 then
      make_plot (Matrix.rem_row 0 soln_matrix);
    soln_matrix |> Matrix.cols |> List.rev |> List.hd
