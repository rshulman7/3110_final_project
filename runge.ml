(** Implementation of the Runge–Kutta method for solving first-order
    linear ODEs. *)
type elt = Reals.t

type v = Vector.t

type t = Matrix.t

exception Invalid_end_time

exception Invalid_step_size

let sing_eq_rk m init_cond end_time step_size = failwith "Unimplemented"
