type move_phrase = {
  value : int;
  row : int;
  col : int;
}

type command =
  | Move of move_phrase
  | Quit

exception Empty
exception Malformed

val parse : string -> command
