(** This module contains all functions related to the Gaussian
    elimination algorithm and QR factorization algorithm. *)

type elt = Reals.t

type v = Vector.t

type t = Matrix.t

(** [norm ?norm_type m] is the [?norm_type] norm of [m]. The default
    [?norm_type] is "2", which represents the 2-norm.

    requires: [?norm_type] is one of "2", "1", "op" *)
val norm : ?norm_type:string -> t -> elt

(** [rref m b] is [m | b] row reduced into echelon form to solve the
    system of linear equations mx = b. Specifically, all pivots are 1. *)
val rref : t -> v -> t

(** [mat_exp m] is the matrix exponential of matrix [m] *)
val mat_exp : t -> t

(** [det m] is the determinant of matrix [m] *)
val det : t -> elt
