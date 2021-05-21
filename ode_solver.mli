(** this module contains various solvers for ode's *)

type m = Matrix.t

type elt = Reals.t

type v = Vector.t

exception Invalid_end_time

exception Invalid_step_size

val exact_linear_solver : m -> m -> elt -> v

val rk : bool -> m -> v -> elt -> elt -> v

(** [euler m init_cond end_time step_size] returns the ending solution
    vector of the ODE and also makes a plot of the solution curve if it
    is a single equation system

    solves a system of first-order linear ODEs, for example given the
    equation x' = ax + d, y' = bx + c the matrix m =
    [\[a; 0; d\], \[b; 0 ; c\]] init_cond is a vector of all initial
    conditions

    Raises: Invalid_end_time if end_time <= t_0. Raises:
    Invalid_step_size if step_size <= 0. *)
val euler : bool -> m -> v -> elt -> elt -> v
