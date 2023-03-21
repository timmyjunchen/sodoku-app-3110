type t

exception InvalidBox of string
exception InvalidAnswer of string

val start_board : t -> int list list
val print_board : int list list -> unit
val get_row : int list list -> int -> int list
val get_col : int list list -> int -> int list
val get_block : int list list -> int * int -> int list
