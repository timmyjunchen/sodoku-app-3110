exception InvalidBox of string
exception InvalidAnswer of string

let rec _print_list lst =
  match lst with
  | [] -> ()
  | h :: t ->
      print_int h;
      print_string " ";
      _print_list t

let print_board (board : int list list) : unit =
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
      board
  in
  let result = List.fold_left ( ^ ) "" string_rows in
  let _ = print_endline result in
  ()

let get_row (board : int list list) (rownum : int) : int list =
  List.nth board (rownum - 1)

let get_col (board : int list list) (colnum : int) : int list =
  List.fold_left ( @ ) []
    (List.map (fun row -> [ List.nth row (colnum - 1) ]) board)

let get_block (board : int list list) ((blockrow, blockcol) : int * int) :
    int list =
  List.fold_left ( @ ) []
    (List.mapi
       (fun i row ->
         if i >= (blockrow - 1) * 3 && i < blockrow * 3 then
           List.filteri
             (fun j cell ->
               j >= (blockcol - 1) * 3 && j < (blockcol * 3) + (cell * 0))
             row
         else [])
       board)
