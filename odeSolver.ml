(** this module contains various solvers for ode's *)

open Reals
open Vector
open Matrix
open LinAlg
open Plot
open Format

type m = Matrix.t

type elt = Reals.t

type v = Vector.t

exception Invalid_end_time

exception Invalid_step_size

let exact_linear_solver mat vec_init time =
  let idx_last_col = snd (size mat) - 1 in
  let nonhom_vec =
    col_at_index mat idx_last_col
    |> Array.to_list |> of_reals_list
    |> (fun x -> [ x ])
    |> of_vector_list |> transpose
  in
  let mat = rem_col idx_last_col mat in
  let eigenvals, eigenvectors = eig mat in
  let diagonal_mat =
    eigenvals |> List.map (( *: ) time) |> List.map exp |> create_diag
  in
  let vec_init = [ vec_init ] |> of_vector_list |> transpose in
  let v_final =
    vec_init
    |> multiply (inverse eigenvectors)
    |> multiply diagonal_mat |> multiply eigenvectors |> transpose
    |> real_list_list_of_matrix
  in
  let nonhom_term =
    eigenvals
    |> List.map (fun lambda ->
           (Rational (1, 1) -: exp (~-:lambda *: time)) /: lambda)
    |> create_diag |> multiply diagonal_mat
    |> fun x ->
    multiply x nonhom_vec |> transpose |> real_list_list_of_matrix
  in
  match (v_final, nonhom_term) with
  | [ v1 ], [ v2 ] -> Vector.(sum (of_reals_list v1) (of_reals_list v2))
  | _ -> failwith "why did this happen"

let apply_funcs adder func rows prev_values =
  match func with
  | Sin ->
      adder :=
        !adder +: Float (sin (Reals.float_of_real prev_values.(rows)))
  | Cos ->
      adder :=
        !adder +: Float (cos (Reals.float_of_real prev_values.(rows)))
  | Exp -> adder := !adder +: exp prev_values.(rows)
  | rat_or_fl -> adder := !adder +: (rat_or_fl *: prev_values.(rows))

let apply_coefs_k1 coefs_for_row prev_values num_of_elts =
  let adder = ref Reals.Zero in
  for rows = 0 to num_of_elts do
    if rows <> num_of_elts then
      apply_funcs adder coefs_for_row.(rows) rows prev_values
    else adder := !adder +: coefs_for_row.(rows)
  done;
  !adder

let apply_sine_k adder prev_value step_size kth multiplier =
  adder :=
    !adder
    +: Float
         (sin
            (float_of_real
               (prev_value +: (step_size *: kth *: multiplier))))

let apply_cos_k adder prev_value step_size kth multiplier =
  adder :=
    !adder
    +: Float
         (cos
            (float_of_real
               (prev_value +: (step_size *: kth *: multiplier))))

let apply_funcs_k adder func rows prev_values kth step_size multiplier =
  match func with
  | Sin ->
      apply_sine_k adder prev_values.(rows) step_size kth multiplier
  | Cos -> apply_cos_k adder prev_values.(rows) step_size kth multiplier
  | Exp ->
      adder :=
        !adder
        +: exp (prev_values.(rows) +: (step_size *: kth *: multiplier))
  | rat_or_fl ->
      adder :=
        !adder
        +: rat_or_fl
           *: (prev_values.(rows) +: (step_size *: kth *: multiplier))

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
      apply_funcs_k adder coefs_for_row.(rows) rows prev_values kth
        step_size multiplier
    else adder := !adder +: coefs_for_row.(rows)
  done;
  !adder

let create_k1_k2_k3_4 coefs_for_row prev_values num_rows step_size =
  let k1 = apply_coefs_k1 coefs_for_row prev_values num_rows in
  let k2 =
    apply_coefs_k2_3_4 coefs_for_row prev_values num_rows k1 step_size
      (Reals.Rational (1, 2))
  in
  let k3 =
    apply_coefs_k2_3_4 coefs_for_row prev_values num_rows k2 step_size
      (Reals.Rational (1, 2))
  in
  let k4 =
    apply_coefs_k2_3_4 coefs_for_row prev_values num_rows k3 step_size
      (Reals.Float 1.)
  in
  (k1, k2, k3, k4)

let rk_step_computation prev_value step_size k1 k2 k3 k4 =
  prev_value
  +: Reals.Rational (1, 6)
     *: step_size
     *: (k1 +: (Reals.Float 2.0 *: k2) +: (Reals.Float 2.0 *: k3) +: k4)

let rk_update
    rows
    num_rows
    col
    prev_values
    coef_matrix
    step_size
    soln_matrix =
  for rows = 1 to num_rows do
    let prev_value = prev_values.(rows - 1)
    and coefs_for_row = Matrix.row_at_index coef_matrix (rows - 1) in
    let k1, k2, k3, k4 =
      create_k1_k2_k3_4 coefs_for_row prev_values num_rows step_size
    in
    Matrix.change_matrix_value rows col
      (rk_step_computation prev_value step_size k1 k2 k3 k4)
      soln_matrix
  done;
  ()

let rk_computation coef_matrix soln_matrix num_rows col step_size =
  let prev_time = Matrix.lookup soln_matrix (0, col - 1) in
  Matrix.change_matrix_value 0 col (prev_time +: step_size) soln_matrix;
  let prev_values_and_time =
    Matrix.col_at_index soln_matrix (col - 1)
  in
  let prev_values =
    Array.sub prev_values_and_time 1
      (Array.length prev_values_and_time - 1)
  in
  rk_update rows num_rows col prev_values coef_matrix step_size
    soln_matrix;
  ()

let check_times_and_step_size end_time step_size =
  if step_size <: Reals.Zero then raise Invalid_step_size
  else if end_time <: Reals.Zero then raise Invalid_end_time

let make_soln_plots plot init_cond soln_matrix =
  if plot && Vector.dim init_cond = 1 then make_plot soln_matrix;
  if plot && Vector.dim init_cond = 2 then
    make_plot (Matrix.rem_row 0 soln_matrix);
  ()

let rk plot m init_cond end_time step_size =
  check_times_and_step_size end_time step_size;
  let num_cols =
    end_time /: step_size |> float_of_real |> Float.ceil |> int_of_float
  and num_rows_of_soln_matrix = fst (Matrix.size m) + 1 in
  let soln_matrix =
    Matrix.make_matrix num_rows_of_soln_matrix (num_cols + 1) Reals.Zero
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
  make_soln_plots plot init_cond soln_matrix;
  soln_matrix |> Matrix.cols |> List.rev |> List.hd

let euler_computation coef_matrix soln_matrix num_rows col step_size =
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
    let adder = apply_coefs_k1 coefs_for_row prev_values num_rows in
    Matrix.change_matrix_value rows col
      (prev_value +: (step_size *: adder))
      soln_matrix
  done;
  ()

let euler plot m init_cond end_time step_size =
  check_times_and_step_size end_time step_size;
  let num_cols =
    end_time /: step_size |> float_of_real |> Float.ceil |> int_of_float
  and num_rows_of_soln_matrix = fst (Matrix.size m) + 1 in
  let soln_matrix =
    Matrix.make_matrix num_rows_of_soln_matrix (num_cols + 1) Reals.Zero
  in
  for row = 1 to num_rows_of_soln_matrix - 1 do
    Matrix.change_matrix_value row 0
      (Vector.lookup init_cond (row - 1))
      soln_matrix
  done;
  for col = 1 to num_cols do
    euler_computation m soln_matrix
      (num_rows_of_soln_matrix - 1)
      col step_size
  done;
  make_soln_plots plot init_cond soln_matrix;
  soln_matrix |> Matrix.cols |> List.rev |> List.hd
