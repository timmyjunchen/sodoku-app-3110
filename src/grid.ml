type t = { start_board : int list list }

let board_setup board = { start_board = board }
let start_board board = board.start_board
