open Types
open Grid_3d
open State

(*[string_of_player p] is the string representation of [p]*)
let string_of_player p = match p with
  | Python -> "python"
  | Caml -> "caml"
  | None -> "none"

let string_of_mode m = match m with
  | Normal -> "normal"
  | Krazy -> "krazy"

let string_of_num_p n = match n with
  | Single -> "single"
  | Multi -> "multi"

let string_of_level l = match l with
  | Easy -> "easy"
  | Medium -> "medium"
  | Hard -> "hard"

(*[random_cell_for_krazy st] is a random cell from the occupied cells in the board
 * specified by [st]*)
let random_cell_for_krazy st =
  let all_cells = cells_occupied (board st) in
  if ((List.length all_cells) = 0) then failwith "Populate your board first!"
  else if ((List.length all_cells) = 1 ) then List.nth (all_cells) 0
  else
    begin
    let index = (Random.int ((List.length  all_cells)-1)) in
    (List.nth (all_cells) index)
  end

(*[krazy_recalc_helper cellst st] is a helper function for krazy_recalc_score*)
let rec krazy_recalc_helper cellst st =
match cellst with
| [] -> st
| h::t -> let mod_st = {st with current_player = player_at_cell h} in
    let move = cell_coords h in
    let com = Place move in
    let st' = do' com mod_st in
    krazy_recalc_helper t st'

(*[krazy_recalc_score st] is a state with updated scores that correctly correspond
  * to the board specified by [st]*)
let krazy_recalc_score st =
  let curr_p_st = curr_player st in
  let num_p = string_of_num_p (game_num_plyrs st) in
  let p1_av = string_of_player (p1_avatar st) in
  let lvl = string_of_level (game_level st) in
  let mode = string_of_mode (game_mode st) in
  let info_str = num_p^" "^p1_av^" "^lvl^" "^mode in
  let occupied = cells_occupied (board st) in
  let st' = init_state info_str in
  let new_st =
    {st' with
      p1_num_hints = st.p1_num_hints;
      p1_num_tries = st.p1_num_tries;
      p2_num_hints = st.p2_num_hints;
      p2_num_tries = st.p2_num_tries} in
  let new_st2 = krazy_recalc_helper occupied {new_st with moves_made = st.moves_made} in
  {new_st2 with current_player = curr_p_st}

(*[krazy_disappearing_sqs st c] is [st] after the move at [c] has been removed*)
let krazy_disappearing_sqs st c =
  Hashtbl.replace st.tttBoard c.cell {c with player = None};
  {st with tttBoard = st.tttBoard} |> krazy_recalc_score

(*[krazy_cell_swap st c1 c2] is [st] after the moves at [c1] and [c2] have been swapped*)
let krazy_cell_swap st c1 c2 =
  let orig_b = board st in
  let b = copy orig_b in
  let c1_player = player_at_cell c1 in
  let c2_player = player_at_cell c2 in
  Hashtbl.replace b (cell_coords c1) {c1 with player = c2_player};
  Hashtbl.replace b (cell_coords c2) {c2 with player = c1_player};
  {st with tttBoard = b} |> krazy_recalc_score

(*[krazy_switch_planes st pl1 pl2] is [st] after planes [pl1] and [pl2] have been switched*)
let krazy_switch_planes st pl1 pl2 =
  let b = board st in
  let copy_b = copy empty_board in
  Hashtbl.iter (fun (p, r, c) v ->
    if p = pl1 then Hashtbl.replace copy_b (pl2, r, c) {v with cell = (pl2, r, c)}
    else if p = pl2 then Hashtbl.replace copy_b (pl1, r, c) {v with cell = (pl1, r, c)}
    else Hashtbl.add copy_b (p, r, c) v) b;
  Hashtbl.remove copy_b (1, 1, 1);
  {st with tttBoard = copy_b} |> krazy_recalc_score

(*[krazy_bomb st c] is [st] after a bomb has gone off at [c]*)
let krazy_bomb st c =
  let orig_b = board st in
  let b = copy orig_b in
  let col_3d = find_vertical_cells c b in (*cell list list*)
  let v_3d_diag = List.flatten (vertical_3d_groups c b) in (*cell list list*)
  let h_3d_diag = List.flatten (horizontal_3d_group c b) in (*cell list list*)
  let plane_2d_inst = List.flatten (three_row_2d_cells c b) in (*cell list list *)
  let instances = col_3d @ v_3d_diag @ h_3d_diag @ plane_2d_inst in
  let inst_list = extract_cell_pos instances in
  let _ = List.iter (fun i ->
      Hashtbl.replace b (Hashtbl.find b i).cell
        {(Hashtbl.find b i) with player=None}) inst_list in
  {st with tttBoard = b} |> krazy_recalc_score

(*[inc_movces st] is [st] with the number of moves made so far incremented*)
let inc_moves st = {st with moves_made = st.moves_made + 1}

(*[up_krazy_happ boole st] is [st] with krazy_happ set to [boole]*)
let up_krazy_happ boole st = {st with krazy_happ = boole}

let do_krazy c st =
  let st' =
    begin
    match c with
    | Try (_, _, _) -> do' c st
    | _ -> do' c st |> inc_moves
    end |> up_krazy_happ false
  in
  let new_st =
    if st'.krazy_bomb_happ then
      {st' with krazy_bomb_happ = false}
    else st' in
  (*disappearing square*)
  if new_st.moves_made = new_st.move_num_dispr then
    let rand_cell = random_cell_for_krazy new_st in
    krazy_disappearing_sqs new_st rand_cell |> up_krazy_happ true

  (*swapping cells*)
  else if new_st.moves_made = new_st.move_num_swap then
    let rand_cell1 = random_cell_for_krazy new_st in
    let rand_cell2 = random_cell_for_krazy new_st in
    krazy_cell_swap new_st rand_cell1 rand_cell2 |> up_krazy_happ true

  (*switching planes*)
  else if new_st.moves_made = new_st.move_num_switch_pl then
    let pl1 = (Random.int 3) in
    let rand2 = (Random.int 3) in
    let pl2 = if pl1 = rand2 then abs (2 - pl1) else rand2 in
    krazy_switch_planes new_st pl1 pl2 |> up_krazy_happ true

  (*bomb*)
  else if new_st.moves_made = new_st.move_num_bomb then
    let rand_cell = random_cell_for_krazy new_st in
    let bomb_state = krazy_bomb new_st rand_cell |> up_krazy_happ true in
    {bomb_state with krazy_bomb_happ = true}

  else new_st
