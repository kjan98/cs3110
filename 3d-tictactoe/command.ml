exception InvalidCommand
open Types

let list_coords obj = try (String.split_on_char ',' obj
      |> List.map (fun a -> String.trim a)
      |> List.map (fun a -> int_of_string a))
  with
  | _ -> raise InvalidCommand

let list_to_tup lst = match lst with
                      | a::b::c::[] -> (a, b, c)
                      | _ -> raise InvalidCommand

let parse str =
  let st = String.lowercase_ascii str |> String.trim in
  let space = if String.contains st ' ' then String.index st ' '
    else String.length st in
  let action = String.trim(String.sub st 0 space) in
  let obj = if space < String.length st then String.trim(String.sub st space ((String.length st) - space))
  else ""
  in
  match action with
  | "play" -> Play obj
  | "start" -> Play str
  | "place" ->
    begin
      try Place (obj |> list_coords |> list_to_tup)
      with
        | InvalidCommand -> Invalid
    end
  | "try" ->
    begin
      try Try (obj |> list_coords |> list_to_tup)
      with
        | InvalidCommand -> Invalid
    end
  | "quit"-> Quit
  | "look" -> Look
  | "score" -> Score
  | "current player" -> CurrentPlayer
  | "restart" -> Restart
  | "hint" -> Hint
  | _ -> Invalid
