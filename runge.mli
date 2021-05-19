(** This module contains all functions related to the Runge–Kutta
    method for numerically solving linear ODEs. *)

type elt = Reals.t

type v = Vector.t

type t = Matrix.t

exception Invalid_end_time

exception Invalid_step_size

val sing_eq_rk : t -> v -> elt -> elt -> v
