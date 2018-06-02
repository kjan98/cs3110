open Types
(**
  * ai is the main engine for the artificial intelligence aspect of the game.
  * This is mainly accessed when the player type "hint", and when the state of the game is recieved
  * or when the game is played in 'single player' mode, and depending on the level ('easy', 'medium', or 'hard')
  * the game tree will choose the sub-optimal to optimal position for the next move on the game board and
  * thus return a new valid state. This will be implemented using Min and Max search and alpha-beta pruning
 **)

(*[easy_ai_move st] is the move the ai makes given the game is in state [st] with
 * the level being easy. Returns a random move*)
val easy_ai_move: state -> command

(*[medium_ai_move st] is the move the ai makes given the game is in state [st] with
 * the level being medium. Returns a move based on a game tree search*)
val medium_ai_move: state -> command

(*[hard_ai_move st] is the move the ai makes given the game is in state [st] with
 * the level being hard. Returns a move based on the result of a minimax algorithm
 * with alpha-beta pruning*)
val hard_ai_move: state -> command

(*[player_hint st] is hint given to the player given the game is in state [st].
 * The quality of the hint is inversely proportional to the level of the game.*)
val player_hint: state -> command
