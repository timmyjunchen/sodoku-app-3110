(** Representation of dynamic sudoku state.

    This module represents the state of the sudoku grid as it is being played,
    including the player's current grid, the boxes that have been filled, and
    functions that cause the state to change. *)

exception InvalidBox of int * int
exception InvalidAnswer of int

type t
(** The abstract type of values representing the game state. *)

val init_state : Grid.t -> t
(** [init_state g] is the initial state of the game when playing the sudoku grid
    [g] game. In that state the player has not yet filled out any boxes and is
    on the starting grid. *)

val current_board : t -> int list list
(** [current_board st] is the identifier of the current board state [st]. *)

(** The type representing the result of an attempted inserting number into box. *)
type result =
  | Legal of t
  | Illegal

val print_board : t -> unit
val get_row : t -> int -> int list
val get_col : t -> int -> int list
val get_block : t -> int * int -> int list
val get_cell : t -> int * int -> int

val next_grid : t -> int -> int -> int -> int list list
(** [answer number grid st] is the result of attempting to answer with number
    [n] in box with row [row] and column [col] in state [st] and grid [grid]:

    - If [n] is a valid integer (Between 1 and 9), and box with [row][col] is
      empty, then the result is [Legal st'], where in [st'] the grid has updated
      with the number filled in the box.

    - Otherwise, the result is [Illegal].

    Effects: none. In particular, [answer] does not print anything. *)
(*val answer : int -> int -> int -> Grid.t -> t -> result *)
