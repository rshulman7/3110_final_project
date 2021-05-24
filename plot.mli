(** This module handles all things related to plotting graphs and
    solutions to the ODEs*)

type t = Matrix.t

exception Invalid_plot

(** [make_plot m] plots m with the first row x-values and second row
    y-values

    Raises: [Invalid_plot] if dimensions of the input [m] are not 2 x n
    where n > 1*)
val make_plot : t -> unit
