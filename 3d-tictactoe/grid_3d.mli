open Parse_init
open Types

(*[empty_board] is an empty 3D tic tac toe board*)
val empty_board: board

(*[fst' c] returns the first entry of the triple [c]*)
val fst' : (int*int*int) -> int

(*[snd' c] returns the second entry of the triple [c]*)
val snd' : (int*int*int) -> int

(*[thd c] returns the third entry of the triple [c]*)
val thd : (int*int*int) -> int

(*[get_cell coords b] is the cell at coordinates [coords] in board [b]*)
val get_cell: (int*int*int) -> board -> cell

(*[cell_coords c] is the coordinates of the cell [c]*)
val cell_coords: cell -> (int*int*int)

(*[player c] is the player at cell [c]*)
val player_at_cell: cell -> player

(*[place c b plyr] modifies board so that the cell at coordinate [c] contains [plyr]
 * raise: InvalidCell failure if [c] is not valid cell*)
val place : (int*int*int) -> board -> player -> unit

(*[asciiBoard b] is the ascii string representation of the 3D tictactoe
 * board [b]
 * NOTE: for testing before implementation of GUI*)
val asciiBoard : board -> string

(*[copy b] is a copy of the board [b]*)
val copy: board -> board

(* [win_evaluation c b] determines whether new three-in-a-row instances
   were found overall in board [b] after putting a move on cell [c].
*)
val win_evaluation: cell -> board -> bool

(* [cells_left b] keeps track of the cells in the 3d grid space that
   have not been played by a player. The accumulating list of cells would be of type
   [cell list] and you take in a board.
*)
val cells_left: board -> cell list

(* [is_taken cell b] determines whether [cell] is an available move in board
   [b]*)
val is_taken: (int * int * int) -> board -> bool

(* [cells_occupied b] keeps track of the cells in the 3d grid space that
   have been played by a player. The accumulating list of cells would be of type
   [cell list] and you take in a board.
*)

val cells_occupied: board -> cell list

(*[get_the_win c current_player b] returns the cells that are of a newly
  found three-in-a-row instance including [c]
*)
val get_the_win: cell -> player -> board -> cell list list

(*[three_row_2d_cells c lst_of_cells] is the list of lists of cells
 *that create a three in a row with [c] in [lst_of_cells]*)
val all_three_in_row_cells: cell -> board -> cell list list

(*[threed_diag_wins c b]*)
val threed_diag_wins: cell -> board -> cell list list

(*[diag_check c b] finds the respective diagonal and horizontal wins that cover
  all three levels of the grid_space that relate to cell [c]
 *)
val diag_check: cell -> board -> (winType3D * winType3D)

(*[three_row_2d_cells c b] compiles a list of 3-in-a-row instances that are
  possible for a given cell [c] in board [b]; 3-in-a-row instances include
  vertical, horizontal, and diagonal matches.
*)
val three_row_2d_cells: cell -> board -> cell list list

(*[victory_on_plane c possible_instances acc] traverses through [possible_instances]
  to check whether at least one of [possible_instances] has resulted in a win
  for the player who has played [c] and accumulates it to [acc]
*)
val victory_on_plane: cell -> cell list list -> cell list list -> cell list list

(*[threed_col_win] actually returns the list of cells that form a column win
  with [c] on the board [b]
*)
val threed_col_win: cell -> board -> cell list

(*[col_check c b] checks to see whether all the remaining cells in the column of
  [c] are taken by the same player that played [c] in [b]
*)
val col_check: cell -> board -> bool

(*[find_vertical_cells c b] finds the other cells that are in the same column
  as [c]
*)
val find_vertical_cells: cell -> board -> cell list

(*[vertical_3d_group c b] extracts the diagonal instances that [c] is part of,
  by slicing the 3D grid space represented by hashtable board [b] vertically
*)
val vertical_3d_groups: cell -> board -> cell list list

(*[horizontal_3d_group c b] extracts the diagonal instances that [c] is part of,
  by slicing the 3D grid space represented by hashtable board [b] horizontally
*)
val horizontal_3d_group: cell -> board -> cell list list

(*[extract_cell_pos inst] takes a three-in-a-row instance and returns a list
  of coordinates that indicate all the positions of the cells of [inst]
*)
val extract_cell_pos: cell list -> (int*int*int) list
