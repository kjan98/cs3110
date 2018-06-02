open Ai
open State
open Types

type stats = {python: int; caml: int; none: int}

(*[string_of_player p] is the string representation of [p]*)
let string_of_player p = match p with
  | Python -> "Python"
  | Caml -> "Caml"
  | None -> "None"

let string_stats stats =
  "Python wins: "^(string_of_int (stats.python))^"\n"^
  "Caml wins: "^(string_of_int (stats.caml))^"\n"^
  "Ties: "^(string_of_int (stats.none))^"\n"

(*[play st] is the helper function for play_game ()*)
let rec play ai1 ai2 (f: string -> int -> stats -> unit) str n st acc_stats=
  if game_ended st then
    begin
<<<<<<< HEAD
    print_endline "]";
    (* print_endline "Game end"; *)
    let win_plyr = (*winner st*) st.winner in
=======
    let win_plyr = winner st in
>>>>>>> ab21e001975288e1e7e8e17ac14f7ff8f28beb75
    let new_stats =
    begin
    match win_plyr with
    | Python -> {acc_stats with python = acc_stats.python + 1}
    | Caml -> {acc_stats with caml = acc_stats.caml + 1}
    | _ -> {acc_stats with none = acc_stats.none + 1}
    end
    in
    f str (n - 1) new_stats
    end
  else
    begin
    let current = curr_player st in
    let move =
      begin
        match current with
        | Python -> ai1 st
        | Caml -> ai2 st
        | None -> failwith "No current player"
      end
    in
    let newSt = do' move st in
    if newSt <> st then
      play ai1 ai2 f str n newSt acc_stats
    end

let rec play_game ai1 ai2 str n acc =
  let init_st = init_state str in
<<<<<<< HEAD
  print_endline ("Starting game " ^ (string_of_int n) ^ "...");
  (* print_board init_st; *)
  print_string "[" |> flush_all;
=======
>>>>>>> ab21e001975288e1e7e8e17ac14f7ff8f28beb75
  if n > 0 then
  try(
    play ai1 ai2 play_game str n init_st acc
  ) with
  | _ -> print_endline "Error!"
  else
    print_endline (string_stats acc)

let rec start ai1 ai2 n =
  play_game ai1 ai2 "multi python easy normal" n {python = 0; caml = 0; none = 0}
<<<<<<< HEAD

(* let () = start easy_ai_move hard_ai_move 1000 *)
=======
>>>>>>> ab21e001975288e1e7e8e17ac14f7ff8f28beb75
