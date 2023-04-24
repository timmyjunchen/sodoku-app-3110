open Grid
(*open Printf*)

exception InvalidBox of int * int
exception InvalidAnswer of int

type t = { current_board : int array array }

let init_state board = { current_board = start_board board }
let current_board st = st.current_board
(*let rec _print_array arr = Array.iter (printf "%d ") arr*)

let print_board (st : t) : unit =
  let _ = print_endline "" in
  let string_of_row (row : int array) : string =
    let temp =
      Array.mapi
        (fun i cell ->
          (if cell <> 0 then string_of_int cell else ".")
          ^ if (i + 1) mod 3 = 0 && i <> 0 && i <> 8 then " | " else "  ")
        row
    in
    Array.fold_left ( ^ ) " " temp
  in
  let string_rows =
    Array.mapi
      (fun i row ->
        string_of_row row
        ^ (if (i + 1) mod 3 = 0 && i <> 8 then "\n---------+---------+--------"
          else "")
        ^ "\n")
      (current_board st)
  in
  let result = Array.fold_left ( ^ ) "" string_rows in
  let _ = print_endline result in
  ()

let get_row (st : t) (rownum : int) : int array =
  (current_board st).(rownum - 1)

let get_col (st : t) (colnum : int) : int array =
  Array.init
    (Array.length (current_board st))
    (fun row -> (current_board st).(row).(colnum - 1))

let get_block (st : t) ((blockrow, blockcol) : int * int) : int array =
  let grid_edge = Array.length (current_board st) in
  let block_edge = int_of_float (sqrt (float_of_int grid_edge)) in
  Array.init grid_edge (fun cell ->
      let row_value = ((blockrow - 1) * block_edge) + (cell / block_edge) in
      let col_value = ((blockcol - 1) * block_edge) + (cell mod block_edge) in
      (current_board st).(row_value).(col_value))

let get_cell (st : t) ((row, col) : int * int) : int =
  (current_board st).(row - 1).(col - 1)

let check_input input = 1 <= input && input <= 9
let replace arr row col value = arr.(row - 1).(col - 1) <- value

let next_grid st row col value =
  let current_board = current_board st in
  match get_cell st (row, col) with
  | 0 -> begin
      match check_input value with
      | true ->
          replace current_board row col value;
          current_board
      | false -> raise (InvalidAnswer value)
      | exception _ -> raise (InvalidAnswer value)
    end
  | exception _ -> raise (InvalidBox (row, col))
  | _ -> raise (InvalidBox (row, col))

type result =
  | Legal of t
  | Illegal

let answer row col value st =
  try
    let new_grid = next_grid st row col value in
    let st' = { current_board = new_grid } in
    Legal st'
  with InvalidBox _ | InvalidAnswer _ -> Illegal
