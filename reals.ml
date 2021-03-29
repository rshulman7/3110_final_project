(** AF: A real is either zero, a rational number, or a decimal expansion

    RI: [Rational (a, b)] only valid when [a] is not zero and [b] is not
    zero. Real a only valid when a is not zero. [Float a] is only valid
    when [a <> 0.] *)

type t =
  | Zero
  | Rational of (int * int)
  | Float of float

exception Invalid_real

exception Ill_defined of string

let rep_ok = function
  | Zero -> Zero
  | Rational (a, b) ->
      if a <> 0 && b <> 0 then Rational (a, b) else raise Invalid_real
  | Float a -> if a <> 0. then Float a else raise Invalid_real

let check_zero a =
  match a with
  | Zero -> Zero
  | Rational (0, b) -> Zero
  | Float 0. -> Zero
  | _ -> a

let float_of_real = function
  | Zero -> 0.
  | Rational (a, b) ->
      (* could remove the if-else based on RI*)
      if b = 0 then raise Division_by_zero
      else float_of_int a /. float_of_int b
  | Float a -> a

(** [numdem a] is [(num, dem)] where [a] is of the form
    [Rational (num,dem)]

    requires: [a] is a Rational *)
let numdem = function
  | Rational (num, dem) -> (num, dem)
  | _ -> failwith "numdem can only be applied to rationals"

let op_on_floats op a b = Float (op (float_of_real a) (float_of_real b))

let float_equality_tol = 1e-7

(** eculid ()*)
let gcd (a, b) =
  let a = abs a and b = abs b in
  let lrg, sml = if a >= b then (a, b) else (b, a) in
  let rec helper a b =
    let q = a mod b in
    if q = 0 then b else helper b q
  in
  helper lrg sml

let reduce = function
  | Rational (a, b) ->
      let factor = gcd (a, b) in
      Rational (a / factor, b / factor)
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

let ( -: ) a b =
  (match (a, b) with
  | Zero, _ -> ~-:b
  | _, Zero -> a
  | Rational _, Rational _ -> a +: ~-:b
  | _ ->
      op_on_floats ( -. ) a b
      (* | _ -> Float (float_of_real a -. float_of_real b) *))
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

(** [intpower a n] raises integer [a] to the integer [n]th power *)
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
  (match b with
  | Zero ->
      if a = Zero then raise (Ill_defined "zero to the power of zero")
      else a
  | Rational (b, 1) ->
      let num, dem = numdem a in
      Rational (intpow num b, intpow dem b)
  | _ -> op_on_floats ( ** ) a b)
  |> check_zero

let sqrt a = a ^: Rational (1, 2)

let string_of_real = function
  | Zero -> "0"
  | Rational (a, b) ->
      let a, b = Rational (a, b) |> reduce |> numdem in
      if b = 1 then string_of_int a
      else string_of_int a ^ "/" ^ string_of_int b
  | Float a -> string_of_float a
