open Command
open Grid_3d
open Parse_init
open Types

let num_helper lvl =
  match lvl with
  |Easy -> 7
  |Medium -> 5
  |Hard -> 3

let init_state str =
  let i = parse_init str in
  {
    tttBoard = copy empty_board;
    current_player = Python;
    curr_score_1 = 0;
    curr_score_2 = 0;
    mode = mode i;
    num_players = player_num i;
    level = level i;
    p1_avatar = p1_avatar i;
    p1_num_hints = num_helper (level i);
    p1_num_tries = num_helper (level i);
    p2_num_hints = num_helper (level i);
    p2_num_tries = num_helper (level i);
    most_recent_win = [];
    winner = None;
    game_end = false;
    krazy_happ = false;
    krazy_bomb_happ = false;
    moves_made = 0;
    move_num_dispr =
      Random.int 26 + 5;
    move_num_swap =
      Random.int 26 + 5;
    move_num_switch_pl =
      Random.int 26 + 5;
    move_num_bomb =
      Random.int 26 + 5
  }

let game_mode s = s.mode

let game_num_plyrs s = s.num_players

let game_level s = s.level

let p1_score s = s.curr_score_1

let p2_score s = s.curr_score_2

let curr_player s = s.current_player

let num_hints st =
  if (st.p1_avatar = Python && st.current_player = Python) ||
  (st.p1_avatar = Caml && st.current_player = Caml) then
    st.p1_num_hints
  else
    st.p2_num_hints

let num_tries st =
  if (st.p1_avatar = Python && st.current_player = Python) ||
  (st.p1_avatar = Caml && st.current_player = Caml) then
    st.p1_num_tries
  else
    st.p2_num_tries

let get_result s = s.winner

let get_result_message s =
  match s.num_players with
  | Multi -> begin
    match s.winner with
    | Caml -> ("caml","Caml wins!")
    | Python -> ("python","Python wins!")
    | None -> ("none", "Draw! No one won")
    end
  | Single -> if s.winner = s.p1_avatar then
      ("win", "Congratulations! You won the Java cup!")
    else if s.winner <> None then
      ("draw", "Sad! You didn't win the Java cup, but try again next time for
that steaming mug of Java!")
    else
      ("lost", "Oh no! You were close to winning the Java cup!")

let rec find_cell s (pl, x, y) = get_cell (pl, x, y) s.tttBoard


(*[make_move s coords] places a move at the cell at coordinates [coords]
 * for the current player of state [s]. Modifies [s]*)
let make_move s (pl, x, y) = place (pl, x, y) s.tttBoard s.current_player

(*[make_move s coords] places a move at the cell at coordinates [coords]
 * for the current player of state [s] but doesn't actually modify
 * [s] itself*)
let try_move s (pl, x, y) = let copy_b = copy s.tttBoard in
  place (pl, x, y) copy_b s.current_player; s

let board s = s.tttBoard

let p1_avatar s = s.p1_avatar

(*[inc_point st] increments the score of the current player of state [st]*)
let inc_point inc_amt st =
  if (st.p1_avatar = Python && st.current_player = Python) ||
    (st.p1_avatar = Caml && st.current_player = Caml) then
    {st with curr_score_1 = st.curr_score_1 + inc_amt}
  else
    {st with curr_score_2 = st.curr_score_2 + inc_amt}

(*[diag_cells_in_question diag_list] is the coordinates of the cells in [diag_list]
 * if [diag_list] has length of 3. Otherwise, return empty list.*)
let diag_cells_in_question diag_list =
  match diag_list with
  | [] -> []
  | (h1:cell)::h2::h3::[] -> (h1.cell)::(h2.cell)::(h3.cell)::[]
  | _ -> []

(*[check diag_cells_in_question h] is whether all the coords in [diag_cells_in_question]
 * are equal to [h]*)
let rec check diag_cells_in_question h =
  match diag_cells_in_question with
  | [] -> true
  | head::tail -> (head = h) && (check tail h)

(*[search st diag_cells_in_q diags_cell_list_only] is whether all the cells in
 * [diag_cells_in_q] are equivalent to those in [diags_cell_list_only]*)
let rec search st diag_cells_in_q diags_cell_list_only =
  match (diags_cell_list_only:(int*int*int) list list) with
  | h::t -> (check diag_cells_in_q h) || (search st diag_cells_in_q t)
  | _ -> false

(*[play_move st coords] is the new state as a result of the current player of state [st]
 * making a move at coordinates [coords], updating the scores accordingly if the move
 * creates a three-in-a-row for the player that made the move*)
let play_move st (pl, row, col) =
  make_move st (pl, row, col);
  if win_evaluation (find_cell st (pl, row, col)) st.tttBoard then
    begin
    let c = find_cell st (pl,row,col) in
    let b = st.tttBoard in
    let diag_check_lst = threed_diag_wins c b in
    let instances = (three_row_2d_cells c b) in
    let case_2d = victory_on_plane c instances [] in
    let case_3d =
    begin
      if (threed_col_win c b) = [] then
        diag_check_lst
      else (threed_col_win c b)::diag_check_lst
    end
    in
    let inced_st = inc_point ((List.length case_2d) + (List.length case_3d)) st in
    let lst_win_coords = List.map (fun lst -> List.map (fun a -> a.cell) lst) (case_2d @ case_3d) in
    {inced_st with most_recent_win = lst_win_coords}
    end
  else
    {st with most_recent_win = []}

let krazy_happ_st st = st.krazy_happ

let krazy_bomb_happ_st st = st.krazy_bomb_happ

let most_recent_wins st = st.most_recent_win

let cells_occ st = cells_occupied (board st)

let other_player ply =
  match ply with
  | Python -> Caml
  | Caml -> Python
  | None -> None

(*[switch_players st] returns the opponent of the current player of state [st]*)
let switch_players st = let _p1_av = st.p1_avatar in
match st.current_player with
| Python -> {st with current_player = Caml}
| _ -> {st with current_player = Python}

(*[check_game_end st] returns [st] with whether the game it specifies has ended
 * updated accordingly*)
let check_game_end st =
  if cells_left st.tttBoard = [] then
    if st.curr_score_1 > st.curr_score_2 then
      {st with winner = st.p1_avatar; game_end = true}
    else if st.curr_score_2 > st.curr_score_1 then
      {st with winner = other_player st.p1_avatar; game_end = true}
    else
      {st with game_end = true}
  else
    st

let game_ended st = st.game_end

let do' c st =
  match c with
  | Play str -> st
  | Score -> st
  | Quit -> st
  | Restart -> st
  | Try (pl, row, col) ->
    begin
      try(
        let st = if (st.current_player = Python) then
            (if (st.p1_num_tries > 0) then {st with p1_num_tries = st.p1_num_tries - 1 }
             else {st with p1_num_tries = 0} )
          else
            (  if (st.p2_num_tries > 0) then {st with p2_num_tries = st.p2_num_tries - 1 }
          else {st with p2_num_tries = 0})  in
        try_move st (pl, row, col)
      )with
      | _ -> st
    end
  | Place (pl, row, col) ->
    begin
      try(
        play_move st (pl, row, col) |> switch_players |> check_game_end
      )with
      | _ -> st
    end
  | Hint ->
    if (st.p1_avatar = Python && st.current_player = Python) ||
      (st.p1_avatar = Caml && st.current_player = Caml) then
      if st.p1_num_hints > 0 then
        {st with p1_num_hints = st.p1_num_hints - 1}
      else
        st
    else
      if st.p2_num_hints > 0 then
        {st with p2_num_hints = st.p2_num_hints - 1}
      else
        st
  | Look -> st
  | CurrentPlayer -> st
  | Invalid -> st
