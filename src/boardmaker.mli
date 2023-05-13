val shuffle : int list -> int list
val generate_random_diagonal : unit -> int array array
val generate_board : unit -> int array array
val valid_place : int array array -> int -> int -> int -> bool
val board_filled : int array array -> bool
val get_options : int array array -> int -> int -> int list
val get_block_options : int array array -> int -> int -> int list array
(* val generate_block : int array array -> int -> int -> int array option *)
