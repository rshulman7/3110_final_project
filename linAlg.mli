(** This module supports essential linear algebra operations. *)

(** The type of elements of matrices. *)
type elt = Reals.t

(** The type of vectors (columns/rows of matrices). *)
type v = Vector.t

(** The type of matrices. *)
type t = Matrix.t

(** Raised when algorithms that fail to converge sufficiently quickly. *)
exception Timeout of string

(** [rref m b] is [m | b] row reduced into echelon form to solve the
    system of linear equations mx = b. Specifically, all pivots are 1. *)
val rref : t -> v -> t

(** [eig m] returns the eigenvalues and eigenvectors of [m].

    Raises: [Timeout] if convergence isn't reached. *)
val eig : t -> elt list * t
