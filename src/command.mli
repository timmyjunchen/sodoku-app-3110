(** Parsing of player commands.

    Parts of the specifications are adapted from A2 *)

type move_phrase = {
  row : int;
  col : int;
  value : int;
}
(** The type [move_phrase] represents the move phrase that can be part of a
    player command following the command [Move]. The record consists of the
    first element being the [row], second element [col], and third element
    [value] of the answer for filling in a cell. The record is in the same order
    as the words in the original player command. For example:

    If the player command is ["place 1 1 1"], then the object phrase is
    [Move { row = 1; col = 1; value = 1 }]. *)

type coordinates = {
  row : int;
  col : int;
}

(** The type [coordinates] represents the coordinates that can be part of a
    player command following the command [delete]. The record consists of the
    first element being the [row] and second element [col] of the cell to delete
    a value. The record is in the same order as the words in the original player
    command. For example:

    If the player command is ["delete 1 1"], then the object phrase is
    [Delete { row = 1; col = 1 }]. *)

(** The type [command] represents a player command that is decomposed into a
    verb and possibly a move phrase or coordinates. Invariant: the [move_phrase]
    carried by [Move] and the [coordinates] carred by [Delete] must not be
    empty. *)
type command =
  | Move of move_phrase
  | Delete of coordinates
  | Options of coordinates
  | Solve
  | Hint
  | Quit
  | PlayMode
  | SolveMode
  | FileMode
  | Help

exception Empty
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed command is parsed. *)

val parse : string -> command
(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes
    the verb. The rest of the words, if any, become the object phrase or
    coordaintes. Examples:

    - [parse "    place 1 1 1  "] is [Move { row = 1; col = 1; value = 1 }]
    - [parse " delete 1 1 "] is [Delete { row = 1; col = 1 }]
    - [parse "quit"] is [Quit].

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces.

    Raises: [Malformed] if the command is malformed. A command is malformed if
    the verb is neither "place", "delete", "solve", " hint", or "quit" or if the
    verb is "quit", "solve", "or hint" and there is a non-empty phrase, or if
    the verb is "place" or "delete" and there is an empty move
    phrase/coordinates.*)
