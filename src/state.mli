(** Representation of dynamic sudoku state.

    This module represents the state of the sudoku grid as it is being played,
    including the player's current grid, the boxes that have been filled, and
    functions that cause the state to change. *)

exception InvalidBox of int * int
exception InvalidAnswer of int

type t
(** The abstract type of values representing the game state. *)

exception UnsolvableBoard of t

val init_state : int array array -> t
(** [init_state g] is the initial state of the game when playing the sudoku grid
    [g] game. In that state the player has not yet filled out any boxes and is
    on the starting grid. *)

val start_board : t -> int array array
(** [start_board st] is the identifier of the start board state [st]. *)

val current_board : t -> int array array
(** [current_board st] is the identifier of the current board state [st]. *)

(** The type representing the result of an attempted inserting number into box. *)
type result =
  | Legal of t
  | Illegal

val deep_copy_state : t -> t
val print_board : t -> unit
val get_row : t -> int -> int array
val get_col : t -> int -> int array
val get_block : t -> int * int -> int array
val get_cell : t -> int * int -> int
val next_grid : t -> int -> int -> int -> int array array
val answer : int -> int -> int -> t -> result
val delete : int -> int -> t -> result

(* [answer row col value grid st] is the result of attempting to answer with
   number [n] in box with row [row] and column [col] in state [st]:

   - If [n] is a valid integer (Between 1 and 9), and box with [row][col] is
   empty, then the result is [Legal st'], where in [st'] the grid has updated
   with the number filled in the box.

   - Otherwise, the result is [Illegal].

   Effects: none. In particular, [answer] does not print anything. *)

val check_win : t -> bool
val solve_board : t -> t
val board_hint : t -> result
