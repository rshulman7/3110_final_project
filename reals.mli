(** Representation of real numbers. Includes standard operations on real
    numbers as well. *)

(** The type that represents real numbers and basic functions

    AF: A real is either zero, a rational number, a decimal expansion,
    or either the cosine, sine or exponential function.

    RI: [Rational (a, b)] only valid when [a <> 0] and [b <> 0].
    [Float a] is only valid when [a <> 0.]. *)
type t =
  | Zero
  | Rational of (int * int)
  | Float of float
  | Sin
  | Cos
  | Exp

(** Raised when an operation is ill-defined (such as $0^0$). *)
exception Ill_defined of string

(** [a =: b] is true iff the two real numbers or functions are equal and
    false otherwise.

    Requires: [a] and [b] are one of [Zero], [Rational], [Float] *)
val ( =: ) : t -> t -> bool

(** [a <: b] is true iff $a<b$.

    Requires: [a] and [b] are one of [Zero], [Rational], [Float] *)
val ( <: ) : t -> t -> bool

(** [cpm_real a b] is [-1] if $a<b$, [0] if [a]=[b] and [1] if [a]>[b].

    Requires: [a] and [b] are one of [Zero], [Rational], [Float]*)
val cmp_real : t -> t -> int

(** [a +: b] is the sum of [a] and [b].

    Requires: [a] and [b] are one of [Zero], [Rational], [Float]. *)
val ( +: ) : t -> t -> t

(** [~-: a] is unary negation of [a].

    Requires: [a] and [b] are one of [Zero], [Rational], [Float]. *)
val ( ~-: ) : t -> t

(** [a -: b] is difference between [a] and [b] (i.e. [a]-[b]).

    Requires: [a] and [b] are one of [Zero], [Rational], [Float]. *)
val ( -: ) : t -> t -> t

(** [a *: b] is [a] times [b].

    Requires: [a] and [b] are one of [Zero], [Rational], [Float]. *)
val ( *: ) : t -> t -> t

(** [ a /: b] is [a] divided by [b].

    Raises: Division_by_zero if [b] is [Zero].

    Requires: [a] and [b] are one of [Zero], [Rational], [Float]. *)
val ( /: ) : t -> t -> t

(** [a ^: b] is [a] to the power of [b].

    Raises: Ill_defined if both [a] and [b] are zero.

    Requires: [a] and [b] are one of [Zero], [Rational], [Float]. *)
val ( ^: ) : t -> t -> t

(** [exp a] is exp([a]).

    Requires: [a] is one of [Zero], [Rational], [Float]. *)
val exp : t -> t

(** [sqrt a] is the square root of [a].

    Requires: [a] is one of [Zero], [Rational], [Float]. *)
val sqrt : t -> t

(** [abs a] is the absolute value of [a].

    Requires: [a] is one of [Zero], [Rational], [Float]. *)
val abs : t -> t

(** [float_of_real a] is an ocaml float value representing the real [a].

    Requires: [a] is one of [Zero], [Rational], [Float]. *)
val float_of_real : t -> float

(** [string_of_real a] is:

    - "0" if [a = Zero]
    - "i" if [a = Rational (a,b)] can be reduced to [Rational (i,1) ]
    - "p/q" if [a = Rational (a,b)] can be reduced to [Rational (p,q) ]
    - [string_of_float a'] if [a = Float a']
    - "sin" if [a = Sin]
    - "cos" if [a = Cos]
    - "exp" if [a = Exp] *)
val string_of_real : t -> string
