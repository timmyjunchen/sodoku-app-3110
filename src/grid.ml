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
