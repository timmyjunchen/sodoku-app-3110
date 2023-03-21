exception InvalidBox of string
exception InvalidAnswer of string

val print_board : int list list -> unit
val get_row : int list list -> int -> int list
val get_col : int list list -> int -> int list
val get_block : int list list -> int * int -> int list
