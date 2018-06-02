type level = Easy| Medium| Hard

type player = Caml | Python| None

type num_players = Single| Multi

type mode = Normal | Krazy

type info = {
  info_players_num: num_players;
  info_mode : mode;
  info_p1_avatar : player;
  info_level  : level;
}

type cell = {cell: (int*int*int); player: player}

type winType3D =
  | WinV of cell list list
  | WinH of cell list list
  | WinNone

type board = ((int*int*int), cell) Hashtbl.t

type state = {
  tttBoard   : board;
  current_player  : player;
  curr_score_1 : int;
  curr_score_2 : int;
  num_players: num_players;
  mode  : mode;
  level : level;
  p1_avatar : player;
  p1_num_hints : int;
  p1_num_tries : int;
  p2_num_hints : int;
  p2_num_tries : int;
  most_recent_win: (int*int*int) list list;
  winner: player;
  game_end: bool;
  krazy_happ: bool;
  krazy_bomb_happ: bool;
  moves_made: int;
  move_num_dispr: int;
  move_num_swap: int;
  move_num_switch_pl: int;
  move_num_bomb: int
}

type command =
  |Play of string
  |Score
  |Quit
  |Restart
  |Try of (int * int * int)
  |Place of (int * int * int)
  |Hint
  |Look
  |CurrentPlayer
  |Invalid
