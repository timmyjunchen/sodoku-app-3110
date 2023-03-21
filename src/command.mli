type move_phrase = {
  row : int;
  col : int;
  value : int;
}

type command =
  | Move of move_phrase
  | Quit

exception Empty
exception Malformed

val parse : string -> command
