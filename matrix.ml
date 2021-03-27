(** AF: a matrix is represented as a length m ordered list of vectors
    each of length n, which we think of as the columns of the matrix,
    together with a length n ordered list of vectors each of length m,
    which we think of as the rows of the matrix.

    RI: list of length m exclusively contains vectors of length n, and
    the list of length n exculisively contains vectors of length m *)

type elt = Reals.t

type v = Vector.t

type index = int * int

(* rows, columns, number of rows, number of columns*)
type t = v list * v list * int * int

exception Invalid_matrix of string

exception Dimension_mismatch of int * int

exception Out_of_bounds

let size ((_, _, a, b) : t) = (a, b)

(* isn't this rows, based on line 15? also real_list_list of matrix on
   line 74 does seem to print rows when I pass it into a printer. so I
   think it is rows.- RES*)
let rows ((r, _, _, _) : t) = r

let cols ((_, c, _, _) : t) = c

let rep_ok t =
  let open Vector in
  let n = t |> size |> fst and m = t |> size |> snd in
  if
    t |> rows |> List.fold_left (fun a b -> a && dim b == m) true
    && t |> cols |> List.fold_left (fun a b -> a && dim b == n) true
  then t
  else raise (Invalid_matrix "failed RI check")

(** [transverse ll] converts a list of lists to a list of lists by
    combining the ith element of each list into a new list, in order.

    requires: all lists in [ll] are of the same length *)
let transverse (ll : 'a list list) =
  let rec helper rows cols =
    match (cols, rows) with
    | [], h :: t -> h |> List.map (fun a -> [ a ]) |> helper t
    | _, h :: t -> h |> List.map2 (fun a b -> b :: a) cols |> helper t
    | _, [] -> cols
  in
  helper (List.rev ll) []

let rlst_of_vlst : Vector.t list -> Reals.t list list =
  List.map (fun a -> Vector.to_reals_list a)

let vlst_of_rlst : Reals.t list list -> Vector.t list =
  List.map (fun a -> Vector.of_reals_list a)

let of_real_list_list rll : t =
  match rll with
  | [] -> raise (Invalid_matrix "tried to create an empty matrix")
  | h :: t ->
      if List.(fold_left (fun a b -> a && length b == length h)) true t
      then
        ( vlst_of_rlst rll,
          vlst_of_rlst (transverse rll),
          List.length rll,
          List.length h )
      else raise (Invalid_matrix "Rows are not all the same length!")

let of_vector_list (v_lst : v list) =
  v_lst |> rlst_of_vlst |> of_real_list_list

(* same as code on line 27 - RES *)
(* let to_vector_list ((a, _, _, _) : t) = a *)

let real_list_list_of_matrix m = rlst_of_vlst (rows m)

let transpose ((rows, cols, nr, nc) : t) : t = (cols, rows, nc, nr)

let add_row vec m = of_vector_list (rows m @ [ vec ])

let add_column m vec = m |> transpose |> add_row vec |> transpose

let same_dims ((_, _, nr1, nc1) : t) ((_, _, nr2, nc2) : t) =
  if nr1 <> nr2 then raise (Dimension_mismatch (nr1, nr2))
  else if nc1 <> nc2 then raise (Dimension_mismatch (nc1, nc2))

let helper_elt_wise (op : Vector.t -> Vector.t -> Vector.t) m1 m2 : t =
  same_dims m1 m2;
  ( List.map2 (fun a b -> op a b) (rows m1) (rows m2),
    List.map2 (fun a b -> op a b) (cols m1) (cols m2),
    fst (size m1),
    fst (size m2) )

let sum = helper_elt_wise Vector.sum

let scalar_mult (e : elt) (m : t) : t =
  let r, c, nr, nc = m in
  ( List.map (fun r -> Vector.scalar_mult e r) r,
    List.map (fun c -> Vector.scalar_mult e c) c,
    nr,
    nc )

let multiply ((r1, c1, nr1, nc1) : t) ((r2, c2, nr2, nc2) : t) : t =
  let rec row_mult_all_cols row cols =
    match cols with
    | [] -> []
    | h :: t -> [ Vector.dot row h ] @ row_mult_all_cols row t
  in
  let rec row_mult_get_full_row_lst rows cols =
    match rows with
    | [] -> []
    | h :: t ->
        [ row_mult_all_cols h cols ] @ row_mult_get_full_row_lst t cols
  in
  if nc1 <> nr2 then raise (Dimension_mismatch (nc1, nc2))
  else row_mult_get_full_row_lst r1 c2 |> of_real_list_list

let multiply ((rows, _, _, nc) : t) ((_, cols, nr, _) : t) : t =
  if nc <> nr then raise (Dimension_mismatch (nc, nr))
  else
    let row_mult row = List.map (Vector.dot row) cols in
    List.map row_mult rows |> of_real_list_list

let mult_elt_wise = helper_elt_wise Vector.mult_elt_wise

let subtract = helper_elt_wise Vector.subtract

(* do we start indexing at (0,0) or the natural (1,1)?*)
let lookup ((r, c, nr, nc) : t) ((a, b) : index) : elt =
  Vector.lookup (List.nth r a) b

(* different types of norms!! How do we account for this? *)
let norm ?norm_type:(norm = "2") (t : t) : elt =
  failwith "Unimplemented"

let rref (t : t) : t = failwith "Unimplemented"

let mat_exp (t : t) : t = failwith "Unimplemented"

let det (t : t) : elt = failwith "Unimplemented"

let matrix_equality ((r1, c1, nr1, nc1) : t) ((r2, c2, nr2, nc2) : t) :
    bool =
  let rec check_vector_lst v1_lst v2_lst =
    match (v1_lst, v2_lst) with
    | [], [] -> true
    | [], _ -> false
    | _, [] -> false
    | h1 :: t1, h2 :: t2 ->
        Vector.vector_equality h1 h2 && check_vector_lst t1 t2
  in
  check_vector_lst r1 r2 && check_vector_lst c1 c2 && nr1 = nr2
  && nc1 = nc2

let to_string ((r1, c1, nr1, nc1) : t) : string =
  let rec printer v_lst =
    match v_lst with
    | [] -> ""
    | h :: t -> Vector.string_of_vector h ^ "; \n" ^ printer t
  in
  printer r1
