let print_block (board : int list list) : unit =
  let string_of_row (row : int list) : string =
    let temp =
      List.map
        (fun cell -> (if cell <> 10 then string_of_int cell else " ") ^ " | ")
        row
    in
    List.fold_left ( ^ ) "| " temp
  in
  let string_rows = List.map (fun row -> string_of_row row ^ "\n") board in
  let result = List.fold_left ( ^ ) "\n" string_rows in
  let _ = print_endline result in
  ()
