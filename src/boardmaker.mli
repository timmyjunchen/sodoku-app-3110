(** Generates a board of given size and difficulty*)

val shuffle : int list -> int list
(** [shuffle lst] is [lst] with the elements of lst in random order. *)

val get_options : int array array -> int -> int -> int list
(** [get_options board row_num col_num] is the int list of possible numbers that
    can go in the cell specified by [row_num] and [col_num]. [row_num] and
    [col_num] begin with index 1, not 0. *)

val generate_board : int -> int -> int array array * int array array
(** [generate_board dim difficulty] is the sudoku board solution and the initial
    board (positions removed) with size [dim] and difficulty [difficulty]. The
    first board is the filled in board (the solution) and the second board is
    the starting board. The higher the difficulty, the fewer pre-filled position
    on the board. Each call to [generate_board] produces a unique board.
    Requires: [dim] is a square number greater than 1 (e.g 4, 9, 16, ...).
    [difficulty] is a number between 1 and 5, inclusive. *)
