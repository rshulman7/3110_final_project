(** This module contains various solvers for ODEs *)

type m = Matrix.t

type elt = Reals.t

type v = Vector.t

exception Invalid_end_time

exception Invalid_step_size

(** [exact_linear_solver m init_v end_time] solves a linear system of
    ODEs (i.e. $x' = Ax + b) by finding eigenvalues and eigenvectors.
    The matrix [m] is n-by-(n+1), where the first n columns represent
    $A$, and the last column represents $b$. The initial condition is
    [v]. The starting time is assumed to be 0 and the end time is
    [end_time]. The function returns the vector $x([end_time])$.

    Raises exceptions if either the algorithm for finding eigenvalues or
    the algorithm for finding eigenvectors fail to converge on the given
    matrix *)
val exact_linear_solver : m -> v -> elt -> v

(** [euler b m init_v end_time step_size] is a finite difference solver
    of the ODE system given by matrix [m] using Euler's method according
    to the end_time and step_size. It returns the ending solution vector
    $x([end_time])$.

    If [b = true] then it also makes a plot of the solution curve if it
    is a single equation system or a two equation system

    Raises: Invalid_end_time if [end_time] <= t_0. Raises:
    Invalid_step_size if step_size <= 0. *)
val euler : bool -> m -> v -> elt -> elt -> v

(** [rk b m init_v end_time step_size] is a finite difference solver of
    the ODE system given by matrix [m] using the Runge-Kutta method
    (RK4) according to the end_time and step_size. It returns the ending
    solution vector $x([end_time])$.

    If [b = true] then it also makes a plot of the solution curve if it
    is a single equation system or a two equation system

    Raises: Invalid_end_time if [end_time] <= t_0. Raises:
    Invalid_step_size if step_size <= 0. *)
val rk : bool -> m -> v -> elt -> elt -> v
