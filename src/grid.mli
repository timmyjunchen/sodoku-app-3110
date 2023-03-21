type t

exception InvalidBox of string
exception InvalidAnswer of string

val board_setup : int list list -> t
val start_board : t -> int list list
val current_board : t -> int list list
val print_board : t -> unit
val get_row : t -> int -> int list
val get_col : t -> int -> int list
val get_block : t -> int * int -> int list
