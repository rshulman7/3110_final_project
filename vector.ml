(** AF: a vector is represented as a [(Real.t list * int)] pair where
    [int] is the dimension of the vector and the [Real.t list] is the
    vector in order

    RI: length of [Real.t list] is the same as [int]. *)

type elt = Reals.t

type t = Vector of elt list * int

exception Dimension_Mismatch

let dim : t -> int = function Vector (_, d) -> d

let from_reals_list elt_lst = Vector (elt_lst, List.length elt_lst)

let to_reals_list = function Vector (elt_lst, _) -> elt_lst

let add_elt (v : t) (e : elt) : t =
  Vector (to_reals_list v @ [ e ], dim v + 1)

let sum (v_1 : t) (v_2 : t) : t =
  if dim v_1 = dim v_2 then
    let reals_1 = to_reals_list v_1 and reals_2 = to_reals_list v_2 in
    Vector
      (List.map2 (fun x y -> Reals.( +: ) x y) reals_1 reals_2, dim v_1)
  else raise Dimension_Mismatch

let dot (v_1 : t) (v_2 : t) : elt =
  if dim v_1 = dim v_2 then
    let reals_1 = to_reals_list v_1 and reals_2 = to_reals_list v_2 in
    let dotted =
      List.map2 (fun x y -> Reals.( *: ) x y) reals_1 reals_2
    in
    let rec sum_plus_acc (acc : elt) (elt_lst : elt list) : elt =
      match elt_lst with
      | [] -> acc
      | h :: t -> sum_plus_acc (Reals.( +: ) acc h) t
    in
    sum_plus_acc Reals.Zero dotted
  else raise Dimension_Mismatch

let scalar_mult (v : t) (e : elt) : t =
  Vector (to_reals_list v |> List.map (fun x -> Reals.( *: ) e x), dim v)

let cross : t -> t -> t = failwith "Unimplemented"

let subtract (v_1 : t) (v_2 : t) : t =
  Reals.Float (-1.) |> scalar_mult v_2 |> sum v_1

let lookup (v : t) (index : int) : elt =
  index |> List.nth (to_reals_list v)

let norm = failwith "Unimplemented"

let to_string : t -> string = failwith "Unimplemented"
