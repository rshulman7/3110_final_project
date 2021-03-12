module type Interface = sig 
  
  (* float or float list *)
  type t 

  val function_opt : string -> t list 

   (*matrix mult -> prompt_for_matrix Operations.matrix_mult mat
   matrix scal -> prompt_for_matrix Operations.matrix_scal
   *)

  val parse_size : string -> int * int

  val num_matrix : string -> int

  val parse_matrix : string -> t list

  val print_matrix : t list -> string

  (** [prompt_for_matrix op] takes in operation [op] and returns 
  
  let x = 
  rparse (read line
  
  in op x *)


 (* funcgtion_applier function_opt vector = *)



end 
(*
{
  one : float list
  two : float list
}

{
  matrix : float list
  1 2 3 
  4 5 6
  7 8 9

  1 2 3 4 5 6 7 8 9
  2 5



}
*)

(*
(** [play_game f] starts the adventure in file [f]. *)
let play_game f = failwith "Unimplemented"

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 Text Adventure Game engine.\n";
  print_endline
    "Please enter the name of the game file you want to load.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Execute the game engine. *)
let () = main ()
*)