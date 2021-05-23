(** AF: A real is either zero, a rational number, or a decimal expansion

    RI: [Rational (a, b)] only valid when [a] is not zero and [b] is not
    zero. Real a only valid when a is not zero. [Float a] is only valid
    when [a <> 0.] *)

type t =
  | Zero
  | Rational of (int * int)
  | Float of float
  | Sin
  | Cos
  | Exp

exception Invalid_real

exception Ill_defined of string

let rep_ok = function
  | Zero -> Zero
  | Rational (a, b) ->
      if a <> 0 && b <> 0 then Rational (a, b) else raise Invalid_real
  | Float a -> if a <> 0. then Float a else raise Invalid_real
  | Sin -> Sin
  | Cos -> Cos
  | Exp -> Exp

let check_zero a =
  match a with
  | Zero -> Zero
  | Rational (0, b) -> Zero
  | Float 0. -> Zero
  | _ -> a

let float_of_real = function
  | Zero -> 0.
  | Rational (a, b) ->
      if b = 0 then raise Division_by_zero
      else float_of_int a /. float_of_int b
  | Float a -> a
  | Sin | Cos | Exp -> failwith "cannot operate on functions"

(** [numdem a] is [(num, dem)] where [a] is of the form
    [Rational (num, dem)]

    requires: [a] is a Rational *)
let numdem = function
  | Rational (num, dem) -> (num, dem)
  | _ -> failwith "numdem can only be applied to rationals"

(** [op_on_floats op a b] is the result of converting [a] and [b] to
    floats, applying [op], and returning a real value.

    Requires: [a] and [b] are one of [Zero], [Rational], [Float]*)
let op_on_floats op a b = Float (op (float_of_real a) (float_of_real b))

let float_equality_tol = 1e-7

(** [gcd (a, b)] is the greates common denominator of [a] and [b].
    Implemented using Euclid's algorithm *)
let gcd (a, b) =
  let a = abs a and b = abs b in
  let lrg, sml = if a >= b then (a, b) else (b, a) in
  let rec helper a b =
    let q = a mod b in
    if q = 0 then b else helper b q
  in
  helper lrg sml

(** [reduce a] is the most reduced form of the rational [a]

    requires: [a] is a [Rational] *)
let reduce = function
  | Rational (a, b) ->
      let factor = gcd (a, b) in
      if factor <> 0 then Rational (a / factor, b / factor)
      else Rational (a, b)
  | _ -> failwith "reduce can only be applied to rationals"

let abs a =
  match a with
  | Zero -> Zero
  | Rational (a, b) -> Rational (abs a, abs b)
  | _ -> Float (a |> float_of_real |> abs_float)

let ( =: ) a b =
  match (a, b) with
  | Zero, Zero -> true
  | Rational (a1, a2), Rational (b1, b2) -> a1 * b2 = a2 * b1
  | Float a, Float b -> Float.abs (a -. b) < float_equality_tol
  | _ -> float_of_real a = float_of_real b

let ( <: ) a b = float_of_real a < float_of_real b

let cmp_real a b =
  match (a <: b, b <: a, a =: b) with
  | true, _, _ -> -1
  | _, true, _ -> 1
  | _, _, true -> 0
  | _ -> failwith "impossible"

let ( +: ) a b =
  (match (a, b) with
  | Zero, _ -> b
  | _, Zero -> a
  | Rational (a1, a2), Rational (b1, b2) ->
      Rational ((a1 * b2) + (b1 * a2), a2 * b2)
  | _ -> op_on_floats ( +. ) a b)
  |> check_zero

let ( ~-: ) = function
  | Zero -> Zero
  | Rational (a1, a2) -> Rational (~-a1, a2)
  | Float a -> Float ~-.a
  | Sin | Cos | Exp -> failwith "cannot operate on functions"

let ( -: ) a b =
  (match (a, b) with
  | Zero, _ -> ~-:b
  | _, Zero -> a
  | Rational _, Rational _ -> a +: ~-:b
  | _ -> op_on_floats ( -. ) a b)
  |> check_zero

let ( *: ) a b =
  (match (a, b) with
  | Zero, _ -> Zero
  | _, Zero -> Zero
  | Rational (a1, a2), Rational (b1, b2) -> Rational (a1 * b1, a2 * b2)
  | _ -> op_on_floats ( *. ) a b)
  |> check_zero

let ( /: ) a b =
  (match (a, b) with
  | _, Zero -> raise Division_by_zero
  | Zero, _ -> Zero
  | Rational _, Rational (b1, b2) -> a *: Rational (b2, b1)
  | _ -> op_on_floats ( /. ) a b)
  |> check_zero

(** [intpow a n] is $a^n$. *)
let intpow a n =
  let rec helper acc a n =
    assert (n >= 0);
    match n with
    | 0 -> acc
    | 1 -> acc * a
    | _ ->
        if n mod 2 = 0 then helper acc (a * a) n / 2
        else helper (acc * a) a (n - 1)
  in
  helper 1 a n

let ( ^: ) a b =
  if a = Sin || a = Cos || a = Exp then
    failwith "cannot operate on functions";
  (match (b, a) with
  | Zero, a ->
      if a = Zero then raise (Ill_defined "zero to the power of zero")
      else a
  | Rational (b, 1), Rational (a1, a2) ->
      Rational (intpow a1 b, intpow a2 b)
  | _ -> op_on_floats ( ** ) a b)
  |> check_zero

let exp a = Float (float_of_real a |> Float.exp)

let sqrt a = a ^: Rational (1, 2)

let string_of_real = function
  | Zero -> "0"
  | Rational (a, b) ->
      let a, b = Rational (a, b) |> reduce |> numdem in
      if b = 1 then string_of_int a
      else if b = -1 then string_of_int (a * -1)
      else if b < 0 then
        string_of_int (a * -1) ^ "/" ^ string_of_int (b * -1)
      else string_of_int a ^ "/" ^ string_of_int b
  | Float a -> string_of_float a
  | Sin -> "sin"
  | Cos -> "cos"
  | Exp -> "exp"
