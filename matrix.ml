open Reals

type elt = Reals.t

type v = Vector.t

(** AF: A matrix is represented as an array of arrays of Reals. The
    inner arrays

    RI: None. *)
type t = elt array array

exception Invalid_matrix of string

exception Dimension_mismatch of int * int

let make_matrix = Array.make_matrix

let change_matrix_value i j elt m = m.(i).(j) <- elt

let size m = (Array.length m, Array.length m.(0))

(** [transverse ll] converts a list of lists to a list of lists by
    combining the ith element of each list into a new list, in order.

    Requires: all lists in [ll] are of the same length *)
let transverse ll =
  let rec helper rows cols =
    match (cols, rows) with
    | [], h :: t -> h |> List.map (fun a -> [ a ]) |> helper t
    | _, h :: t -> h |> List.map2 (fun a b -> b :: a) cols |> helper t
    | _, [] -> cols
  in
  helper (List.rev ll) []

let of_real_list_list rll : t =
  if rll = [ [] ] then
    raise (Invalid_matrix "tried to create an empty matrix")
  else
    let m = Array.of_list rll |> Array.map Array.of_list in
    let len = Array.length m.(0) in
    if Array.for_all (fun a -> Array.length a = len) m then m
    else raise (Invalid_matrix "Rows are not all the same length!")

let real_list_list_of_matrix m : elt list list =
  Array.map Array.to_list m |> Array.to_list

let transpose m =
  let row_len, col_len = size m in
  let new_m = make_matrix col_len row_len Reals.Zero in
  for i = 0 to row_len - 1 do
    for j = 0 to col_len - 1 do
      new_m.(j).(i) <- m.(i).(j)
    done
  done;
  new_m

let rows (m : t) =
  m |> real_list_list_of_matrix |> List.map Vector.of_reals_list

let cols m = m |> transpose |> rows

let row_at_index m ind = m.(ind)

let col_at_index m ind = (transpose m).(ind)

let square m =
  let rows, cols = size m in
  rows = cols

let eye n =
  let new_m = make_matrix n n Reals.Zero in
  for i = 0 to n - 1 do
    new_m.(i).(i) <- Reals.Rational (1, 1)
  done;
  new_m

let create_diag lst =
  let len = List.length lst in
  let new_m = make_matrix len len Zero in
  List.iteri
    (fun idx x ->
      new_m.(idx).(idx) <- x;
      ())
    lst;
  new_m

let diag m =
  if square m then (
    let rows = Array.length m in
    let diags = Array.make rows Reals.Zero in
    for i = 0 to rows - 1 do
      diags.(i) <- m.(i).(i)
    done;
    diags)
  else raise (Failure "not square")

let string_row_to_string row = Array.fold_left (fun a b -> a ^ b) "" row

let to_string m =
  let row_to_string =
    Array.fold_left
      (fun acc x -> acc ^ Reals.string_of_real x ^ ", ")
      ""
  in
  let matrix_entries =
    Array.fold_left (fun acc r -> acc ^ row_to_string r ^ ";\n") "" m
  in
  "[" ^ matrix_entries ^ "]"

let add_row (v : v) (m : t) =
  [ Vector.to_reals_list v ] |> of_real_list_list |> Array.append m

let add_column v m = m |> transpose |> add_row v |> transpose

let rem_row idx m =
  if Array.length m = 1 then
    raise (Failure "tried to remove a row but there's only one row");
  if Array.length m <= idx then
    raise (Failure "tried removing a nonexistent row");
  let len = Array.length m in
  Array.sub m (idx + 1) (len - idx - 1)
  |> Array.append (Array.sub m 0 idx)

let rem_col index m = transpose m |> rem_row index |> transpose

let of_vector_list (v_list : v list) : t =
  List.map Vector.to_reals_list v_list
  |> List.map Array.of_list |> Array.of_list

let elt_wise m1 m2 op =
  let m1_row_len, m1_col_len = size m1 in
  if size m1 <> size m2 then raise (Dimension_mismatch (0, 0))
  else
    let new_m = make_matrix m1_row_len m1_col_len Reals.Zero in
    for i = 0 to m1_row_len - 1 do
      for j = 0 to m1_col_len - 1 do
        new_m.(i).(j) <- op m1.(i).(j) m2.(i).(j)
      done
    done;
    new_m

let sum m1 m2 = elt_wise m1 m2 Reals.( +: )

let scalar_mult e m = Array.map (Array.map (fun x -> x *: e)) m

let multiply m1 m2 =
  let m1_row_len, m1_col_len = size m1
  and m2_row_len, m2_col_len = size m2 in
  if m1_col_len <> m2_row_len then
    raise (Dimension_mismatch (m1_col_len, m2_row_len))
  else
    let new_m = make_matrix m1_row_len m2_col_len Reals.Zero in
    let trans_m2 = transpose m2 in
    for i = 0 to m1_row_len - 1 do
      for j = 0 to m2_col_len - 1 do
        new_m.(i).(j) <-
          Array.map2 (fun x y -> x *: y) m1.(i) trans_m2.(j)
          |> Array.fold_left (fun x y -> x +: y) Reals.Zero
      done
    done;
    new_m

let mult_elt_wise m1 m2 = elt_wise m1 m2 Reals.( *: )

let subtract m1 m2 = elt_wise m1 m2 Reals.( -: )

let lookup m (row, col) = m.(row).(col)

let swap r1 r2 (m : t) =
  let fst = m.(r1) and snd = m.(r2) in
  m.(r1) <- snd;
  m.(r2) <- fst

let matrix_equality (m1 : t) (m2 : t) =
  if size m1 <> size m2 then false
  else
    List.for_all2
      (List.for_all2 Reals.( =: ))
      (real_list_list_of_matrix m1)
      (real_list_list_of_matrix m2)

let rref m =
  try
    let lead = ref 0 and r_len_plus, c_len_plus = size m in
    let r_len = r_len_plus - 1 and c_len = c_len_plus - 1 in
    for row = 0 to r_len do
      if c_len <= !lead then raise (Failure "Stop");
      let i = ref row in
      while lookup m (!i, !lead) = Reals.Zero do
        incr i;
        if r_len = !i then begin
          i := row;
          incr lead;
          if c_len = !lead then raise (Failure "Stop")
        end
      done;
      swap !i row m;
      let new_pivot = lookup m (row, !lead) in
      m.(row) <- Array.map (fun v -> v /: new_pivot) m.(row);
      for i = 0 to r_len do
        if i <> row then
          let new_pivot = m.(i).(!lead) in
          m.(i) <-
            Array.mapi
              (fun i iv -> iv -: (new_pivot *: m.(row).(i)))
              m.(i)
      done;
      incr lead
    done
  with Failure _ -> ()

let rec det m =
  assert (m |> size |> fst = (m |> size |> snd));
  if size m = (1, 1) then m.(0).(0)
  else
    match rows m with
    | h :: t -> List.fold_left Reals.( +: ) Reals.Zero (iterate_det h t)
    | [] -> assert false

and iterate_det h t =
  List.mapi (list_map_det (of_vector_list t)) (Vector.to_reals_list h)

and list_map_det m i a =
  Reals.( *: )
    (if i mod 2 = 0 then a else Reals.(~-:a))
    (det (m |> rem_col i))

let concat : t -> t -> t = Array.map2 (fun a b -> Array.append a b)

let extract_mat idx m =
  let len = snd (size m) in
  Array.map (fun a -> Array.sub a idx (len - idx)) m

let inverse m =
  let nr, nc = size m in
  if nr <> nc then
    raise (Invalid_matrix "cannot invert a matrix that is not square");
  if det m =: Reals.Zero then
    raise (Invalid_matrix "cannot invert singular matrix");
  let inversed = concat m (eye nc) in
  rref inversed;
  extract_mat nc inversed
