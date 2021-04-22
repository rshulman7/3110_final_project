(** AF: a vector is represented as a [(Real.t list * int)] pair where
    [int] is the dimension of the vector and the [Real.t list] is the
    vector in order

    RI: length of [Real.t list] is the same as [int]. *)

type elt = Reals.t

type t = Vector of elt list * int

exception Dimension_mismatch

exception Out_of_bounds

let dim (Vector (_, d)) = d

(* let to_reals_list = function Vector (elt_lst, _) -> elt_lst *)
let to_reals_list (Vector (lst, _)) = lst

let of_reals_list (elt_lst : elt list) =
  Vector (elt_lst, List.length elt_lst)

let add_elt (v : t) (e : elt) : t =
  Vector (to_reals_list v @ [ e ], dim v + 1)

let dim_eq (Vector (_, d1)) (Vector (_, d2)) =
  if d1 <> d2 then raise Dimension_mismatch

let sum (Vector (v1, d1) as vec1) (Vector (v2, d2) as vec2) =
  dim_eq vec1 vec2;
  Vector (List.map2 Reals.( +: ) v1 v2, d1)

let dot (Vector (v1, d1) as vec1) (Vector (v2, d2) as vec2) : elt =
  dim_eq vec1 vec2;
  (* let dotted = List.map2 (fun x y -> Reals.( *: ) x y) reals_1
     reals_2 in List.fold_left Reals.( +: ) Reals.Zero dotted *)
  List.fold_left2
    (fun acc x y -> Reals.(acc +: (x *: y)))
    Reals.Zero v1 v2

let scalar_mult e v =
  Vector (to_reals_list v |> List.map (Reals.( *: ) e), dim v)

let mult_elt_wise (Vector (v1, d1) as vec1) (Vector (v2, d2) as vec2) =
  (* let rec mult reals_lst1 reals_lst2 = match (reals_lst1, reals_lst2)
     with | [], [] -> [] | [], _ -> raise Dimension_mismatch | _, [] ->
     raise Dimension_mismatch | h1 :: t1, h2 :: t2 -> [ Reals.( +: ) h1
     h2 ] @ mult t1 t2 in if dim v1 = dim v2 then Vector (mult
     (to_reals_list v1) (to_reals_list v2), dim v1) else raise
     Dimension_mismatch *)
  dim_eq vec1 vec2;
  Vector (List.map2 Reals.( *: ) v1 v2, d1)

(* do we start indexing at 0 or the natural 1 as this will be used in
   matrix.ml?*)
let lookup v idx : elt =
  if idx >= dim v then raise Out_of_bounds;
  idx |> List.nth (to_reals_list v)

let subtract v1 v2 : t =
  v2 |> scalar_mult (Reals.Rational (-1, 1)) |> sum v1

let cross v_1 v_2 =
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
  else raise Dimension_mismatch

(* Precondition: every element of elt_lst should be >= 0*)
let rec max_of_abs_elt_lst elt_lst =
  match elt_lst with
  | [] -> Reals.Zero
  | h :: t -> max h (max_of_abs_elt_lst t)

(* let max_of_abs_elt_lst = List.fold_left *)

let norm ?(norm_type : string = "2") (v : t) : elt =
  match norm_type with
  | "2" ->
      to_reals_list v
      |> List.map (fun x -> Reals.( *: ) x x)
      |> List.fold_left Reals.( +: ) Reals.Zero
      |> Reals.sqrt
  | "sup" -> to_reals_list v |> List.map Reals.abs |> max_of_abs_elt_lst
  | "1" ->
      to_reals_list v
      |> List.fold_left Reals.(fun a b -> abs a +: abs b) Reals.Zero
  | _ -> failwith "Unknown Norm Type"

let vector_equality (Vector (v1, d1) as vec1) (Vector (v2, d2) as vec2)
    =
  dim_eq vec1 vec2;
  List.fold_left2 (fun b x y -> b && Reals.(x =: y)) true v1 v2

let string_of_vector (v : t) : string =
  let reals_lst = to_reals_list v in
  (*and d = dim v in*)
  let rec printer lst =
    match lst with
    | [] -> ""
    | [ h ] -> Reals.string_of_real h
    | h :: t -> Reals.string_of_real h ^ "; " ^ printer t
  in
  "[" ^ printer reals_lst ^ "]"
(*^ " with dimension " ^ string_of_int d *)
