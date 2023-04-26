(*open Printf*)

exception InvalidBox of int * int
exception InvalidAnswer of int

type t = {
  start_board : int array array;
  current_board : int array array;
}

let init_state board = { start_board = board; current_board = board }
let start_board st = st.start_board
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
  let result =
    Array.init grid_edge (fun cell ->
        let row_value = ((blockrow - 1) * block_edge) + (cell / block_edge) in
        let col_value = ((blockcol - 1) * block_edge) + (cell mod block_edge) in
        (current_board st).(row_value).(col_value))
  in
  let _ =
    print_endline
      (List.fold_left
         (fun acc x -> acc ^ string_of_int x)
         "" (result |> Array.to_list))
  in
  result

let get_cell (st : t) ((row, col) : int * int) : int =
  (* let _ = print_endline (string_of_int row ^ string_of_int col) in let _ =
     print_endline (string_of_int (current_board st).(1).(1)) in *)
  (current_board st).(row - 1).(col - 1)

let check_input input = 1 <= input && input <= 9
(* let replace arr row col value = arr.(row - 1).(col - 1) <- value *)

let next_grid st row col value =
  let current_board = current_board st in
  (* let _ = get_cell st (row, col) |> string_of_int |> print_endline in *)
  let _ = print_board st in
  match get_cell st (row, col) with
  | 0 -> begin
      match check_input value with
      | true ->
          (* replace current_board row col value; *)
          current_board.(row - 1).(col - 1) <- value;
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
    let st' = { start_board = start_board st; current_board = new_grid } in
    Legal st'
  with InvalidBox _ | InvalidAnswer _ -> Illegal

module SS = Set.Make (Int)

let check_valid board row col =
  let arr = board in
  let row_set = ref SS.empty in
  let col_set = ref SS.empty in
  let flag = ref true in
  for i = 0 to 8 do
    if SS.mem arr.(row).(i) !row_set then flag := false
    else row_set := SS.add arr.(row).(i) !row_set
  done;
  for i = 0 to 8 do
    if SS.mem arr.(col).(i) !col_set then flag := false
    else col_set := SS.add arr.(col).(i) !col_set
  done;
  !flag

let solve_board brd =
  let board = brd.current_board in
  let rec helper board =
    try
      for row = 0 to 8 do
        for col = 0 to 8 do
          if row == 8 && col == 8 then raise Exit
          else if board.(row).(col) == 0 then
            for i = 1 to 9 do
              board.(row).(col) <- i;
              if check_valid board row col then helper board else ()
            done
          else ()
        done
      done
    with Exit -> ()
  in
  helper board;
  brd
