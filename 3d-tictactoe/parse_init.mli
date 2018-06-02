open Yojson.Basic.Util
open Types

(*[parse_init f] is the information extract from json file [f]*)
val parse_init: string -> info

(*[mode i] is the number of players for the game as specified by [i]*)
val mode: info -> mode

(*[p1_avatar i] is the avatar of player 1 as specified by [i]*)
val p1_avatar: info -> player

(*[level i] is the level of the game as specified by [i]*)
val level: info -> level

(*[player_num i] is the number of players as specified by [i]*)
val player_num: info -> num_players
