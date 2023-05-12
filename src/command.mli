type coordinates = {
  row : int;
  col : int;
}

type move_phrase = {
  row : int;
  col : int;
  value : int;
}

type command =
  | Move of move_phrase
  | Delete of coordinates
  | Solve
  | Hint
  | Quit

exception Empty
exception Malformed

val parse : string -> command
