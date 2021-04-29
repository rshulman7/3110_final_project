(** this module contains various solvers for ode's *)

open Reals
open Vector
open Matrix
open Linearalgops

type m = Matrix.t

type elt = Reals.t

type v = Vector.t

let exact_linear_solver mat vec_init time =
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
  match v_final with
  | [ h ] -> Vector.of_reals_list h
  | _ -> failwith "impossible"
