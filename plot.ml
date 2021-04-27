open Plplot
open Matrix
open Reals

type t = Matrix.t

(* finding the min so that we can scale the axes accordingly *)
let min_row row = Array.fold_left Float.min row.(0) row

(* finding the max so that we can scale the axes accordingly *)
let max_row row = Array.fold_left Float.max row.(0) row

let make_plot m =
  let xs =
    row_at_index m 0 |> Array.map (fun x -> Reals.float_of_real x)
  in
  let ys =
    row_at_index m 1 |> Array.map (fun x -> Reals.float_of_real x)
  in
  plinit ();

  plenv (min_row xs) (max_row xs) (min_row ys) (max_row ys) 0 0;

  plline xs ys;

  plend ();
  ()
