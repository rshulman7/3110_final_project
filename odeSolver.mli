(** This module contains various solvers for ODEs *)

(** The type of elements of matrices. *)
type elt = Reals.t

(** The type of vectors (columns/rows of matrices). *)
type v = Vector.t

(** The type of matrices. *)
type m = Matrix.t

(** Raised when a solution to and ODE is requested at an invalid time. *)
exception Invalid_end_time

(** Raised when a finite difference method is called with an invalid
    step size. *)
exception Invalid_step_size

(** [exact_linear_solver m init_v end_time] solves a linear system of
    ODEs (i.e. $x' = Ax + b) by finding eigenvalues and eigenvectors.
    The matrix [m] is n-by-(n+1), where the first n columns represent
    $A$, and the last column represents $b$. The initial condition is
    [v]. The starting time is assumed to be 0 and the end time is
    [end_time]. The function returns the vector $x([end_time])$.

    Requires: all entries in [m], and well as [init_v] and [end_time]
    are one of [Zero], [Rational], [Float].

    Raises: exceptions if either the algorithm for finding eigenvalues
    or the algorithm for finding eigenvectors fail to converge on the
    given matrix. *)
val exact_linear_solver : m -> v -> elt -> v

(** [euler b m init_v end_time step_size] is a finite difference solver
    of the ODE system given by matrix [m] using Euler's method according
    to the [end_time] and [step_size]. It returns the ending solution
    vector $x([end_time])$.

    If [b = true] then it also makes a plot of the solution curve if it
    is a single equation system or a two equation system

    Requires: all entries in [init_v] and [end_time] are one of [Zero],
    [Rational], [Float].

    Raises: Invalid_end_time if [end_time] <= t_0. Raises:
    Invalid_step_size if [step_size <= 0]. *)
val euler : bool -> m -> v -> elt -> elt -> v

(** [rk b m init_v end_time step_size] is a finite difference solver of
    the ODE system given by matrix [m] using the Runge-Kutta method
    (RK4) according to the [end_time] and [step_size]. It returns the
    ending solution vector $x([end_time])$.

    If [b = true] then it also makes a plot of the solution curve if it
    is a single equation system or a two equation system.contents

    Requires: all entries in [init_v] and [end_time] are one of [Zero],
    [Rational], [Float].

    Raises: Invalid_end_time if [end_time] <= t_0. Raises:
    Invalid_step_size if [step_size <= 0]. *)
val rk : bool -> m -> v -> elt -> elt -> v
