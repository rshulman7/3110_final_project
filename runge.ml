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

let rk_computations soln_matrix col step_size coef constant =
  let half = Reals.Rational (1, 2) in
  let sixth = Reals.Rational (1, 6) in
  let prev_time = Matrix.lookup soln_matrix (0, col - 1) in
  let prev_value = Matrix.lookup soln_matrix (1, col - 1) in
  let k1 = (coef *: prev_value) +: constant in
  let k2 =
    (coef *: (prev_value +: (step_size *: k1 *: half))) +: constant
  in
  let k3 =
    (coef *: (prev_value +: (step_size *: k2 *: half))) +: constant
  in
  let k4 = (coef *: (prev_value +: (step_size *: k3))) +: constant in
  Matrix.change_matrix_value 0 col (prev_time +: step_size) soln_matrix;
  Matrix.change_matrix_value 1 col
    (prev_value
    +: sixth *: step_size
       *: (k1 +: (Reals.Float 2.0 *: k2) +: (Reals.Float 2.0 *: k3)
         +: k4))
    soln_matrix

let sing_eq_rk plot m init_cond end_time step_size =
  let start_time = Vector.lookup init_cond 0
  and start_value = Vector.lookup init_cond 1 in
  if step_size <: Reals.Zero then raise Invalid_step_size
  else if end_time <: start_time then raise Invalid_end_time
  else
    let num_cols =
      (end_time -: start_time) /: step_size
      |> float_of_real |> Float.ceil |> int_of_float
    in
    let soln_matrix = Matrix.make_matrix 2 (num_cols + 1) Reals.Zero in
    Matrix.change_matrix_value 0 0 start_time soln_matrix;
    Matrix.change_matrix_value 1 0 start_value soln_matrix;
    let coef = Matrix.lookup m (0, 0)
    and constant = Matrix.lookup m (0, 1) in
    for col = 1 to num_cols do
      rk_computations soln_matrix col step_size coef constant
    done;
    if plot then make_plot soln_matrix;
    soln_matrix |> Matrix.cols |> List.rev |> List.hd
