type elt = Reals.t

(** AF: a vector is represented as an array of Reals.t

    RI: None. *)
type t = elt array

exception Dimension_mismatch

exception Out_of_bounds

let dim v = Array.length v

let of_reals_list elt_lst = Array.of_list elt_lst

let to_reals_list v = Array.to_list v

let init = Array.make

let add_elt v e = Array.make 1 e |> Array.append v

(** [elt_wise_op op v1 v2] applies operation [op] on [v1] and [v2]
    elementwise, creating a new vector with the ith entry equal to [op]
    [v1]_i [v2]_i

    Raises: Dimension_mismatch if [dim v1 <> dim v2]. *)
let elt_wise_op op v1 v2 =
  if dim v1 <> dim v2 then raise Dimension_mismatch
  else Array.map2 (fun x y -> op x y) v1 v2

let sum = elt_wise_op Reals.( +: )

let mult_elt_wise = elt_wise_op Reals.( *: )

let dot v1 v2 =
  mult_elt_wise v1 v2 |> Array.fold_left Reals.( +: ) Reals.Zero

let scalar_mult e v = Array.map (fun x -> Reals.( *: ) e x) v

let subtract = elt_wise_op Reals.( -: )

(** [pick_max_abs a b] picks the larger in absolute value between [a]
    and [b]

    Requires: [a] and [b] are both one of [Zero], [Rational], [Float]. *)
let pick_max_abs a b =
  let open Reals in
  let a', b' = (abs a, abs b) in
  if a' <: b' then b' else a'

let norm ?(norm_type : string = "2") (v : t) : elt =
  match norm_type with
  | "2" ->
      Reals.sqrt
        (Array.fold_left
           (fun acc a -> Reals.((a *: a) +: acc))
           Reals.Zero v)
  | "1" ->
      Array.fold_left (fun acc a -> Reals.(abs a +: acc)) Reals.Zero v
  | "sup" -> Array.fold_left pick_max_abs Reals.Zero v
  | _ -> failwith "not a valid norm type"

let vector_equality (v1 : t) (v2 : t) =
  Array.for_all2 Reals.( =: ) v1 v2

let lookup v index = v.(index)

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let string_of_vector (vector : t) : string =
  pp_list Reals.string_of_real (to_reals_list vector)
