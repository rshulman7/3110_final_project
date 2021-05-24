(** This module handles plotting graphs. *)

(** The type of data from which plots are generated. *)
type t = Matrix.t

(** Raised when the data provided doesn't generate a valid plot. *)
exception Invalid_plot

(** [make_plot m] plots m with the first row x-values and second row
    y-values.

    Raises: [Invalid_plot] if dimensions of the input [m] are not 2 x n
    where n > 1. *)
val make_plot : t -> unit
