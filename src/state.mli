(** Representation of dynamic sudoku state and the static board data.

    This module represents the state of the sudoku grid as it is being played,
    including the player's current grid, the starting grid, and functions that
    cause the state to change.

    This module also handles loading of the sudoku board from JSON as well as
    querying information about the sudoku board. *)

exception InvalidBox of int * int
(** Raised when a cell given the row and columns does not exist on the board. It
    carries the row and column number. Example: for a 9 x 9 board, the cell has
    row 10, col 10*)

exception InvalidAnswer of int
(** Raised when a number to a cell is given that is invalid for the board. It
    carries the number of the inputted answer. Example: for a 9 x 9 board, the
    inputted answer is 10. *)

val from_json : Yojson.Basic.t -> int array array
(** [from_json json] is the board that [json] represents. Requires: [j] is a
    valid JSON sudoku board representation. *)

type t
(** The abstract type of values representing the game state. *)

exception UnsolvableBoard
(** Raised when a board is not solvable by the sudoku solver algorithm. Example:
    for a 9 x 9 board, if the given board has two 1s in one row. *)

val init_state : int array array -> t
(** [init_state g] is the initial state of the game when playing the sudoku grid
    [g] game. In that state the player has not yet filled out any cells and is
    on the starting grid. *)

val start_board : t -> int array array
(** [start_board st] is the identifier of the start board state [st]. *)

val current_board : t -> int array array
(** [current_board st] is the identifier of the current board state [st]. *)

(** The type representing the result of an attempted inserting number into the
    cell. *)
type result =
  | Legal of t
  | Illegal

val deep_copy_board : int array array -> int array array
(** [deep_copy_board board] is the int array array that is a deep copy of the
    board [board], which does not change when the inputted board changes. *)

val deep_copy_state : t -> t
(** [deep_copy_state state] is the state that is a deep copy of the state
    [state], which does not change when the inputted state changes. *)

val print_board : t -> unit
(** [print_board state] prints the current board of the inputted state. The int
    array array is printed in the form of a sudoku board. *)

val get_row : t -> int -> int array
(** [get_row state row] is an int array that corresponds to the nth [row] row of
    the current board in state [state].

    Requires: [row] is an integer between 1 and the board_size. (Ex: for 9 x 9,
    row values have to be between 1 and 9)*)

val get_col : t -> int -> int array
(** [get_col state col] is an int array that corresponds to the nth [col] column
    of the current board in state [state].

    Requires: [col] is an integer between 1 and the board_size. (Ex: for 9 x 9,
    column values have to be between 1 and 9) *)

val get_block : t -> int * int -> int array
(** [get_block state block_row block_col] is an int array that corresponds to
    the board block in the at the nth [block_row] block row and nth [block_col]
    block column of the current board in state [state].

    Requires: [block_row] and [block_col] is an integer between 1 and the square
    root of the board_size. (Ex: for 9 x 9, block_row and block_column values
    have to be between 1 and 3) *)

val get_cell : t -> int * int -> int
(** [get_cell state row col] is the int that corresponds to the value in the
    cell that is in the nth [row] row and nth [col] column of the current board
    in state [state].

    Requires: [row] and [col] is an integer between 1 and the board_size. (Ex:
    for 9 x 9, row and column values have to be between 1 and 9) *)

val next_grid : t -> int -> int -> int -> int array array
(** [next_grid state row col value] is the identifier of the board in state
    [state] that is occurs when value [value] is answered in cell with row [row]
    and column [col]. Raises [InvalidBox row col] if row [row] and column [col]
    does not exist on the board. Raises [InvalidAnswer value] if [value] is not
    a valid answer for the board in state [state]. *)

val answer : int -> int -> int -> t -> result
(** [answer row col value state] is the result of attempting to answer with
    value [value] in cell with row [row] and column [col] in state [state]:

    - If [value] is a valid integer (Between 1 and the grid size), and cell with
      [row][col] is empty, then the result is [Legal st'], where in [st'] the
      grid has updated with the number filled in the cell.

    - Otherwise, the result is [Illegal]. *)

val delete : int -> int -> t -> result
(** [delete row col state] is the result of attempting to delete the value in
    cell with row [row] and column [col] in state [state]:

    - If [row] and [col] are in the board and there is a value in the cell that
      was not in the starting board in [state], then the result is [Legal st'],
      where in [st'] the grid has updated with the number deleted frmo the cell.

    - Otherwise, the result is [Illegal]. *)

val check_win : t -> bool
(** [check_win state] is the boolean whether the current board in [state] is
    completely filled in, with all the values following the rules of sudoku (one
    of each value in every row, column and block). *)

val solve_board : t -> t
(** [solve_board state] is the new state with the solved board of the current
    board in state [state]. The new state returns true for [check_win], with
    current_board being completely filled in, with all the values following the
    rules of sudoku *)

val board_hint : t -> result
(** [hint state] is the result of attempting to provide a hint to the current
    board in state [state]:

    - If the current board in [state] is solvable, [Legal st'], where in [st']
      the grid has updated with a correct value filled in to a random cell .

    - Otherwise, the result is [Illegal]. *)
