(** Representation of real numbers as integers, fractions, or decimals

    AF: A real is either zero, a rational number, or a decimal expansion

    RI: [Rational (a, b)] only valid when [a] is not zero and [b] is not
    zero. Real a only valid when a is not zero. [Float a] is only valid
    when [a <> 0.] *)

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

(** [ a /: b] is real [a] divided by real [b]

    raises: Division_by_zero if [b] is [Zero]*)
val ( /: ) : t -> t -> t

(** [a ^: b] is real [a] to the power of real [b]

    raises: Ill_defined if both [a] and [b] are zero *)
val ( ^: ) : t -> t -> t

(** [sqrt a] is the square root of [a] *)
val sqrt : t -> t

(** [abs a] is the absolute value of [a] *)
val abs : t -> t

val string_of_real : t -> string
