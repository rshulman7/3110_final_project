(* Implementation of the Euler method for solving first-order linear
   ODEs *)

type elt = Reals.t

type v = Vector.t

type t = Matrix.t

exception Invalid_end_time

exception Invalid_num_steps

open Reals
open Plot

let sing_eq_euler m init_cond end_time num_steps =
  let start_time = Vector.lookup init_cond 0
  and start_value = Vector.lookup init_cond 1 in
  let step_size = (end_time -: start_time) /: num_steps in
  if num_steps <: Reals.Zero then raise Invalid_num_steps
  else if step_size <: Reals.Zero then raise Invalid_end_time
  else
    let num_cols = int_of_float (float_of_real num_steps) + 1 in
    let soln_matrix = Matrix.make_matrix 2 num_cols Reals.Zero in
    Matrix.change_matrix_value 0 0 start_time soln_matrix;
    Matrix.change_matrix_value 0 1 start_value soln_matrix;
    let coef = Matrix.lookup m (0, 0)
    and constant = Matrix.lookup m (0, 1) in
    for col = 1 to num_cols - 1 do
      let prev_time = Matrix.lookup soln_matrix (0, col - 1) in
      let prev_value = Matrix.lookup soln_matrix (1, col - 1) in
      Matrix.change_matrix_value 0 col
        (prev_time +: step_size)
        soln_matrix;
      Matrix.change_matrix_value 1 col
        (prev_value +: (step_size *: ((coef *: prev_value) +: constant)))
        soln_matrix
    done;
    make_plot soln_matrix;
    soln_matrix |> Matrix.cols |> List.rev |> List.hd
