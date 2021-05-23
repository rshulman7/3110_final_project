type elt = Reals.t

(** AF: a vector is represented as an array of Reals.t

    RI: None. *)
type t = elt array

exception Dimension_mismatch

exception Out_of_bounds

let rep_ok v = v

let dim v = Array.length v

let of_reals_list elt_lst = Array.of_list elt_lst

let to_reals_list v = Array.to_list v

let init = Array.make

let add_elt v e = Array.make 1 e |> Array.append v

let elt_wise_op v1 v2 op =
  if dim v1 <> dim v2 then raise Dimension_mismatch
  else Array.map2 (fun x y -> op x y) v1 v2

let sum v1 v2 = elt_wise_op v1 v2 Reals.( +: )

let mult_elt_wise v1 v2 = elt_wise_op v1 v2 Reals.( *: )

let dot v1 v2 =
  mult_elt_wise v1 v2
  |> Array.fold_left (fun x y -> Reals.( +: ) x y) Reals.Zero

let scalar_mult e v = Array.map (fun x -> Reals.( *: ) e x) v

let subtract v1 v2 = elt_wise_op v1 v2 Reals.( -: )

let norm ?(norm_type : string = "2") (v : t) : elt =
  match norm_type with
  | "2" ->
      Reals.sqrt
        (Array.fold_left
           (fun acc a -> Reals.((a *: a) +: acc))
           Reals.Zero v)
  | "1" ->
      Array.fold_left (fun a acc -> Reals.(abs a +: acc)) Reals.Zero v
  | "sup" -> failwith "unimplemented"
  | _ -> failwith "not a valid norm type"

let vector_equality (v1 : t) (v2 : t) =
  Array.for_all2 Reals.( =: ) v1 v2

let lookup v index = v.(index)

let string_of_vector (v : t) : string =
  let reals_lst = to_reals_list v in
  let rec printer lst =
    match lst with
    | [] -> ""
    | [ h ] -> Reals.string_of_real h
    | h :: t -> Reals.string_of_real h ^ "; " ^ printer t
  in
  "[" ^ printer reals_lst ^ "]"
