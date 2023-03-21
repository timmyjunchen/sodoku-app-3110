exception InvalidBox of string
exception InvalidAnswer of string

type t = {
  start_board : int list list;
  current_board : int list list;
}

let board_setup board = { start_board = board; current_board = board }
let start_board board = board.start_board
let current_board board = board.current_board

let rec _print_list lst =
  match lst with
  | [] -> ()
  | h :: t ->
      print_int h;
      print_string " ";
      _print_list t

let print_board (board : t) : unit =
  let _ = print_endline "" in
  let string_of_row (row : int list) : string =
    let temp =
      List.mapi
        (fun i cell ->
          (if cell <> 0 then string_of_int cell else ".")
          ^ if (i + 1) mod 3 = 0 && i <> 0 && i <> 8 then " | " else "  ")
        row
    in
    List.fold_left ( ^ ) " " temp
  in
  let string_rows =
    List.mapi
      (fun i row ->
        string_of_row row
        ^ (if (i + 1) mod 3 = 0 && i <> 8 then "\n---------+---------+--------"
          else "")
        ^ "\n")
      (current_board board)
  in
  let result = List.fold_left ( ^ ) "" string_rows in
  let _ = print_endline result in
  ()

let get_row (board : t) (rownum : int) : int list =
  List.nth (current_board board) (rownum - 1)

let get_col (board : t) (colnum : int) : int list =
  List.fold_left ( @ ) []
    (List.map (fun row -> [ List.nth row (colnum - 1) ]) (current_board board))

let get_block (board : t) ((blockrow, blockcol) : int * int) : int list =
  List.fold_left ( @ ) []
    (List.mapi
       (fun i row ->
         if i >= (blockrow - 1) * 3 && i < blockrow * 3 then
           List.filteri
             (fun j cell ->
               j >= (blockcol - 1) * 3 && j < (blockcol * 3) + (cell * 0))
             row
         else [])
       (current_board board))
