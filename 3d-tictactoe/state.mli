open Command
open Grid_3d
open Parse_init
open Types

(*[init_state string] is the initial state of the game
 * with information such as the level, number of players, etc.
 * determined by json file with the name [string]
 *)
val init_state: string -> state

(*[game_mode st] is the mode of the game specified by [st]*)
val game_mode: state -> mode

(*[game_num_plyrs st] is the number of players in the game specified by [st]*)
val game_num_plyrs: state -> num_players

(*[game_level st] is the level of the game specified by [st]*)
val game_level: state -> level

(*[p1score st] is player 1's score when the game is in state [st].
 * Player 1 is the human player*)
val p1_score: state -> int

(*[p2score st] is player 2's score when the game is in state [st].
 * Player 2 could either be a second human player or the computer*)
val p2_score: state -> int

(*[curr_player st] is the player whose turn it is to go when game is in
 * state [st]*)
val curr_player: state -> player

(*[num_hints s] is the number of hints that the current player has when
 * game is in state [s]*)
val num_hints: state -> int

(*[num_tries s] is the number of ints that the current player has when the
 * game is in state [s]*)
val num_tries: state -> int

(*[get_result s] is the player that won in the end, None is the game was a draw*)
val get_result: state -> player

(*[get_result_message s] is the message that is displayed when the result of the game
 * is determined*)
val get_result_message: state -> (string * string)

(*[find_cell s coords] is the cells that is at the coordinates [coords]  in the board of the game
 * when the it is in state [s]*)
val find_cell: state -> (int * int * int) -> cell


(*[game_ended s] is whether the game specified by state [s] has terminated*)
val game_ended: state -> bool

(*[other_player ply] is the avatar of the opponent player of [ply]*)
val other_player: player -> player

(*[board st] is information regarding which spots in the board are filled and
 * with which player's move
 * TODO: figure out what type this function should return*)
val board: state -> board

(*[avatars st] is the assignment of the avatars when game is in state in the form
 * [("player1", player1's avatar); ("player2", player2's avatar)*)
val p1_avatar: state -> player

(*[most_recent_win st] is the list of three-in-a-rows associated the most recent
 * action taken to get state [st]*)
 val most_recent_wins: state -> (int*int*int) list list

(*[krazy_happ_st st] is whether the last move has caused something special to happen
 * if the game is in krazy mode*)
val krazy_happ_st: state -> bool

(*[krazy_bomb_happ_st st] is whether the last move has caused a bomb if the
 * game is in krazy mode*)
val krazy_bomb_happ_st: state -> bool

(*[cells_occ st] is the list of cells that are occupied in the board of [st]*)
val cells_occ: state -> cell list

(*[do' c st] is [st'] if executing command [c] in state [st] results
 * in [st']. The following describe the valid commands and the result
 * that [do'] should return for each one when applied to a state [st]
 *  - The “start” (and its alternate "play"), “try”, “place”
 *    and “restart” return an appropriately updated [st'] as described
 *    in the charter, if their object is valid in [st]. If not, then [st']
 *    is equivalent to [st]
 *      + The object of "start" (and "play") are valid if state is NULL
 *        and player has entered valid information for mode of play, avatar
 *        choices, and level.
 *      + The object of "try" and "place" are valid if state is not NULL and
 *        the move is to place at a empty spot in the board
 *      + The object of "restart" is always valid if state is not NULL. If "restart"
 *        is valid, then calling [do'] on command "restart" and any state [st]
 *        returns NULL
 *  - The “quit” command us always possible. This command, along with the
 *    commands "look", “hint” and “score”, leave the observable state
 *    unchanged ([st'] is equivalent to [st]).
 *      + The object "hint" is only valid if there remains an empty spot on
 *        the board and the game has not terminated (a game is defined to have terminated
 *        if there are no empty cells left on the board)
 *      + The object "score" is only valid if it requests the score of a valid player
 *        (NOTE: This is so that the current player will not only be able to see his own
 *        score but also the score of the opponent, whether human or computer)
 *      + The object "look" is only valid before the proper GUI is implemented and for use
 *        with the text GUI (NOTE: Do we need look if we get to the fancier GUI, since then
 *        the board is always in display?)
 * If [c] is not a valid command, [st'] is equivalent to [st]
 * effects: none. do' shouldn't print or display anything in order to maintain
 * model-view-controller
 *)
val do': command -> state -> state
