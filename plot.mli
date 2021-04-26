(** This module handles all things related to plotting graphs and
    solutions to the ODEs*)

type t = Matrix.t

(* [make_plot m] plots m with the first row x-values and second row
   y-values*)
val make_plot : t -> unit
