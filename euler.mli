(** This module contains all functions related to the Forward Euler
    Method for numerically solving linear ODEs. *)

type elt = Reals.t

type v = Vector.t

type t = Matrix.t

exception Invalid_end_time

exception Invalid_step_size

(** [euler m init_cond end_time step_size] returns the ending solution
    vector of the ODE and also makes a plot of the solution curve if it
    is a single equation system

    solves a system of first-order linear ODEs, for example given the
    equation x' = ax + d, y' = bx + c the matrix m =
    [\[a; 0; d\], \[b; 0 ; c\]] init_cond is a vector of all initial
    conditions

    Raises: Invalid_end_time if end_time <= t_0. Raises:
    Invalid_step_size if step_size <= 0. *)
val euler : bool -> t -> v -> elt -> elt -> v
