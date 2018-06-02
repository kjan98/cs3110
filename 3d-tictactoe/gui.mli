open Graphics
open State
open Camlimages
open Types

exception Restart
exception Quit

type choices

(*[init_welcome] Draws the welcome screen, with the title of the game and creators
  RETURNS: a string to start the game *)
val init_welcome : unit -> string

(* [begin_game x y] draws the game screen*)
val begin_game : unit -> unit

(* [cover_up ()] Causes the warning message (stay inside the lines, can't play in
   (1,1,1), etc ) to disappear *)
val cover_up: unit -> unit

(* [play_board command x y] will return the concatenated form of "[command] cell"
   and a tuple of coordinates to determine where the picture should be placed.
   Example of commands include: "place", "try"
*)
val play_board : string -> int -> int -> string * (int * int)

(* [responsive_board x y] Draws the python or caml picture (determined by the
   string parameter) at the location specified by [x] and [y]
*)
val responsive_board: string -> int -> int->  unit

(* [repeat_cell x y] will display an error message depending on the [x] and [y]
   coordinates of where the user touched. These messages include: stay in the
   lines, cell already taken, and forbidden cell *)
val repeat_cell : int -> int -> unit

(* [hightligh_curr_player playerr] Draws a rectangle around the image of the
   current player [playerr] *)
val highlight_curr_player: string -> unit

(* [score p1 p2] Draws the current score for each player. The first int is player1's
   score and the second int is player 2's score
*)
val score : int -> int -> unit

(* [winner_winner_chicken_dinner winner] Displays the win/lose/draw message based
   on what [winner] corresponds to
  *)
val winner_winner_chicken_dinner: string -> unit

(* [num_try_hint num x y] displays the [num] of hints and tries left at location
   [x],[y]
   REQUIRES: int x and int y that is within range of the GUI window
*)
val num_try_hint : int -> int -> int -> unit

(* [cover_try playerr x y] covers the [playerr]'s try at location [x],[y] when
   the user decides to choose a different spot rather than what they had
   originally tried *)
val cover_try : string -> int -> int -> unit

(* [try_responsive_board playerr x y (pl,ex,why)] returns (true, xx, yy) if
   the player accepts the tried move (by either pressing on the accept button or
   the same square)It returns (false, xx yy) if [playerr] decides to not accept
   the location and presses anywhere else. (xx, yy) are the coordinates of where
   the player pressed.
   REQUIRES: string version of a player, int x and int y that is within range
            of the GUI window, a valid cell
*)
val try_responsive_board: string -> int -> int-> (int * int * int)-> (bool* int * int)


(* [which_command ()] Returns which a triple where the first element is the string
   version of command depending on whether or not the user has pressed the try button,
   and the second and third element are the x and y coordinates, respectively of
   where the user pressed
*)
val which_command : unit -> string*int*int

(* [draw_three_row most_recent_wins] draws a red box around the cells in
   most_recent_wins
   REQUIRES: most_recent_wins is a list of the cells that make a three in a row
*)
val draw_three_row : (int*int*int)  list ->  unit

(* [cell_coords_to_x_y cell] returns the (x,y) location of where the playerr's
   icon will be placed according to cell [cell]
   REQUIRES: a valid cell
*)
val cell_coords_to_x_y: (int*int*int) -> (int*int)


(* [get_img filename] returns an image according to [filename] *)
val get_img: string -> image

(* Draws a message asking the user to please wait as the computer calculates
   the best move
*)
val draw_wait_mgs : unit -> unit

(* [draw_act_two playerr p1_score p2_score hint_num num_tries recent_wins lst]
   draws the playing board  with all the necessary information
*)
val draw_act_two : string -> int -> int -> int -> int -> (int*int*int) list list -> unit

(* [bomb_animation x y] takes in the x and y position of the icon of the clicked
   cell and will call a helper function (bomb_boom) and will animate a bomb
   booming
*)
val bomb_animation : unit -> unit

(* [krazy_ocur_animation x y] flashes a message telling the user that something
   crazy happened at location x y and will call a helper function (kray_kray)
   and will aimate a bomb booming
*)
val krazy_ocur_animation : unit -> unit
