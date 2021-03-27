(* Implementation of the linear algebra operations *)

type elt = Reals.t

type v = Vector.t

type t = Matrix.t

(* different types of norms!! How do we account for this? *)
let norm ?norm_type:(norm = "2") (t : t) : elt =
  failwith "Unimplemented"

let switch r1 r2 r =
  if Vector.vector_equality r r1 then r2
  else if Vector.vector_equality r r2 then r1
  else r

(* returns a new matrix that has rows r1 and r2 switched of m*)
let switch_rows (m : t) (r1 : v) (r2 : v) : t =
  Matrix.rows m |> List.map (switch r1 r2) |> Matrix.of_vector_list

let divide_row_by_indexed_elt_of_row row index =
  row
  |> Vector.scalar_mult
       (Reals.( /: ) (Reals.Float 1.) (Vector.lookup row index))

let row_subtract_in_elim row_index first_row =
  List.mapi (fun index row ->
      if index >= 0 then
        Vector.scalar_mult (Vector.lookup row row_index) first_row
        |> Vector.subtract row
      else row)

let forward_elim rows =
  let row_index = ref (-1) in
  let next_row () = row_index := !row_index + 1 in
  let rec reduce_rows row_index rows =
    match rows with
    | [] -> []
    | [ h ] ->
        next_row ();
        [ divide_row_by_indexed_elt_of_row h !row_index ]
    | h :: t ->
        next_row ();
        let new_row = divide_row_by_indexed_elt_of_row h !row_index in
        new_row
        :: reduce_rows row_index
             (row_subtract_in_elim !row_index new_row t)
  in
  reduce_rows row_index rows

let rref (m : t) (v : v) : t =
  let rows = Matrix.add_column m v |> Matrix.rows in
  forward_elim rows |> Matrix.of_vector_list

let mat_exp (t : t) : t = failwith "Unimplemented"

let det (t : t) : elt = failwith "Unimplemented"
