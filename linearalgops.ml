(* Implementation of the linear algebra operations *)

type elt = Reals.t

type v = Vector.t

type t = Matrix.t

type result =
  | Result of t
  | No_result of string
  | Warning of t * string

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

(* divides the row by the indexed element of that row*)
let divide_row_by_indexed_elt_of_row row index =
  row
  |> Vector.scalar_mult
       (Reals.( /: ) (Reals.Float 1.) (Vector.lookup row index))

(* subtracts all rows by first_row to create a column of zeros*)
let row_subtract_in_elim row_index first_row =
  List.map (fun row ->
      Vector.scalar_mult (Vector.lookup row row_index) first_row
      |> Vector.subtract row)

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

let check_solution_rows (m : t) =
  List.fold_left
    (fun cur_res row ->
      match cur_res with
      | No_result _ -> cur_res
      | _ -> (
          match List.rev (Vector.to_reals_list row) with
          | h :: t ->
              if
                not
                  Reals.(
                    Vector.(norm ~norm_type:"1" (of_reals_list t))
                    =: Zero)
              then cur_res
              else if Reals.(h =: Zero) then
                Warning (m, "infinitely many solutions")
              else No_result "there are no solutions"
          | [] -> assert false))
    (Result m) (Matrix.rows m)

(* match List.rev (Matrix.rows m) with | h :: t -> ( match List.rev
   (Vector.to_reals_list h) with | h' :: t' -> if Reals.( Vector.(norm
   ~norm_type:"1" (of_reals_list t')) =: Zero) then 1 else if Reals.(h'
   =: Zero) then 1 else 0 | [] -> assert false) | [] -> assert false *)

let rref (m : t) (v : v) =
  let rows = Matrix.add_column v m |> Matrix.rows in
  forward_elim rows |> Matrix.of_vector_list

(*|> check_solution_rows*)
let backward_solve rows =
  let row_index = ref (List.length rows) in
  let next_row () = row_index := !row_index - 1 in
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
  let rows = Matrix.add_column v m |> Matrix.rows in
  forward_elim rows |> List.rev |> backward_solve |> List.rev
  |> Matrix.of_vector_list

let mat_exp (m : t) : t = failwith "Unimplemented"

let rec det m =
  if Matrix.size m = (1, 1) then Matrix.lookup m (0, 0)
  else
    match Matrix.rows m with
    | h :: t ->
        List.fold_left Reals.( +: ) Reals.Zero
          (List.mapi
             (fun idx a ->
               let _ = print_endline (Matrix.to_string m) in
               Reals.( *: ) a
                 (det Matrix.(t |> of_vector_list |> rem_col idx)))
             (Vector.to_reals_list h))
    | [] -> assert false

(* let get_evals = failwith "Unimplemented" *)
