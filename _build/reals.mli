(** Representation of real numbers as integers, fractions, or decimals

    Some more explanation *)

type t =
  | Zero
  | Rational of (int * int)
  | Float of float

exception Invalid_real

(** [a =: b] is true if the two real numbers are equal and false
    otherwise *)
val ( =: ) : t -> t -> bool

(** [a +: b] is the sum of real numbers [a] and [b] *)
val ( +: ) : t -> t -> t

(** [~-: a] is unary negation of real [a] *)
val ( ~-: ) : t -> t

(** [a -: b] is difference between real [a] and real [b] (i.e. $a-b$) *)
val ( -: ) : t -> t -> t

(** [a *: b] is real [a] multiplied by real [b] *)
val ( *: ) : t -> t -> t

(** [ a /: b] is real [a] divided by real [b] *)
val ( /: ) : t -> t -> t
