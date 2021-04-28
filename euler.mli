(** This module contains all functions related to the Forward Euler
    Method for numerically solving linear ODEs. *)

type elt = Reals.t

type v = Vector.t

type t = Matrix.t

exception Invalid_end_time

exception Invalid_num_steps

(** [euler m init_cond end_time num_steps] returns the ending solution
    vector of the ODE and also makes a plot of the solution curve

    this is a very basic version to deal with single equation
    first-order linear ODEs, thus given the equation x' = cx + d, the
    matrix m = [\[c, d\]] init_cond is a vector [t_0, x(t_0)]

    Raises: Invalid_end_time if end_time <= t_0. Raises:
    Invalid_num_steps if num_steps <= 0. *)
val sing_eq_euler : t -> v -> elt -> elt -> v
