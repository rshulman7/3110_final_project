(** AF: a vector is represented as a [(Real.t list * int)] pair where
    [int] is the dimension of the vector and the [Real.t list] is the
    vector in order

    RI: length of [Real.t list] is the same as [int]. *)

type elt = Reals.t

type t = Vector of elt list * int

exception Dimension_Mismatch

let dim : t -> int = function Vector (_, d) -> d

let from_reals_list (elt_lst : elt list) =
  Vector (elt_lst, List.length elt_lst)

let to_reals_list : t -> elt list = function
  | Vector (elt_lst, _) -> elt_lst

let add_elt (v : t) (e : elt) : t =
  Vector (to_reals_list v @ [ e ], dim v + 1)

let sum (v_1 : t) (v_2 : t) : t =
  if dim v_1 = dim v_2 then
    let reals_1 = to_reals_list v_1 and reals_2 = to_reals_list v_2 in
    Vector
      (List.map2 (fun x y -> Reals.( +: ) x y) reals_1 reals_2, dim v_1)
  else raise Dimension_Mismatch

let rec sum_elts_in_elt_lst (acc : elt) (elt_lst : elt list) : elt =
  match elt_lst with
  | [] -> acc
  | h :: t -> sum_elts_in_elt_lst (Reals.( +: ) acc h) t

let dot (v_1 : t) (v_2 : t) : elt =
  if dim v_1 = dim v_2 then
    let reals_1 = to_reals_list v_1 and reals_2 = to_reals_list v_2 in
    let dotted =
      List.map2 (fun x y -> Reals.( *: ) x y) reals_1 reals_2
    in
    sum_elts_in_elt_lst Reals.Zero dotted
  else raise Dimension_Mismatch

let scalar_mult (e : elt) (v : t) : t =
  Vector (to_reals_list v |> List.map (fun x -> Reals.( *: ) e x), dim v)

let lookup (v : t) (index : int) : elt =
  index |> List.nth (to_reals_list v)

let subtract (v_1 : t) (v_2 : t) : t =
  v_2 |> scalar_mult (Reals.Rational (-1, 1)) |> sum v_1

let cross (v_1 : t) (v_2 : t) : t =
  if dim v_1 = 3 && dim v_2 = 3 then
    let fst_v_1 = lookup v_1 0
    and snd_v_1 = lookup v_1 1
    and thrd_v_1 = lookup v_1 2 in
    let fst_v_2 = lookup v_2 0
    and snd_v_2 = lookup v_2 1
    and thrd_v_2 = lookup v_2 2 in
    let new_v_1 =
      Vector
        ( [
            Reals.( *: ) snd_v_1 thrd_v_2;
            Reals.( *: ) thrd_v_1 fst_v_2;
            Reals.( *: ) fst_v_1 snd_v_2;
          ],
          3 )
    in
    let new_v_2 =
      Vector
        ( [
            Reals.( *: ) thrd_v_1 snd_v_2;
            Reals.( *: ) fst_v_1 thrd_v_2;
            Reals.( *: ) snd_v_1 fst_v_2;
          ],
          3 )
    in
    subtract new_v_1 new_v_2
  else raise Dimension_Mismatch

(* Precondition: every element of elt_lst should be >= 0*)
let rec max_of_abs_elt_lst (elt_lst : elt list) : elt =
  match elt_lst with
  | [] -> Reals.Zero
  | h :: t -> max h (max_of_abs_elt_lst t)

let norm ?(norm_type : string = "2") (v : t) : elt =
  match norm_type with
  | "2" ->
      to_reals_list v
      |> List.map (fun x -> Reals.( *: ) x x)
      |> sum_elts_in_elt_lst Reals.Zero
      |> Reals.sqrt
  | "sup" -> to_reals_list v |> List.map Reals.abs |> max_of_abs_elt_lst
  | _ -> failwith "Unknown Norm Type"

let to_string : t -> string = failwith "Unimplemented"
