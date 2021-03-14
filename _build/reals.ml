(** RI: 
    Rational (a, b) only valid when a is not zero and b is not zero.
    Real a only valid when a is not zero.
     *)

type t = Zero | Rational of (int * int) | Float of float

exception Invalid_real

let rep_ok = function
  | Zero -> Zero
  | Rational (a, b) ->
      if a <> 0 && b <> 0 then Rational (a, b) else raise Invalid_real
  | Float a -> if a <> 0. then Float a else raise Invalid_real

let of_string = failwith "Unimplemented"

let float_of_real = function
  | Zero -> 0.
  | Rational (a, b) ->
      if b = 0 then raise Division_by_zero else float_of_int a /. float_of_int b
  | Float a -> a

let ( =: ) a b =
  match (a, b) with
  | Zero, Zero -> true
  | Rational (a1, a2), Rational (b1, b2) -> a1 * b2 = a2 * b1
  | Float a, Float b -> a = b
  | _ -> false

let reduce (Rational (a, b)) = failwith "Unimplemented"

let ( +: ) a b =
  match (a, b) with
  | Zero, _ -> b
  | _, Zero -> a
  | Rational (a1, a2), Rational (b1, b2) ->
      Rational ((a1 * b2) + (b1 * a2), a2 * b2)
  | _ -> Float (float_of_real a +. float_of_real b)

let ( *: ) a b =
  match (a, b) with
  | Zero, _ -> Zero
  | _, Zero -> Zero
  | Rational (a1, a2), Rational (b1, b2) -> Rational (a1 * b1, a2 * b2)
  | _ -> Float (float_of_real a +. float_of_real b)

let ( /: ) a b =
  match (a, b) with
  | _, Zero -> raise Division_by_zero
  | Zero, _ -> Zero
  | Rational _, Rational (b1, b2) -> a *: Rational (b2, b1)
  | _ -> Float (float_of_real a +. float_of_real b)
