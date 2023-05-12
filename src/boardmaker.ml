(* let rec print_list lst = match lst with | [] -> () | h :: t -> let _ =
   print_int h in print_list t *)

let shuffle lst =
  Random.self_init ();
  let assoc = List.map (fun e -> (Random.int 10000000, e)) lst in
  let shuffled =
    List.sort (fun e1 e2 -> if fst e1 < fst e2 then -1 else 1) assoc
  in
  List.map (fun e -> snd e) shuffled

let fill_box_with_lst board box_row box_col lst =
  for row = (box_row - 1) * 3 to ((box_row - 1) * 3) + 2 do
    for col = (box_col - 1) * 3 to ((box_col - 1) * 3) + 2 do
      board.(row).(col) <- List.nth lst ((row mod 3 * 3) + (col mod 3))
    done
  done;
  board

let print_board (board : int array array) : unit =
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
      board
  in
  let result = Array.fold_left ( ^ ) "" string_rows in
  let _ = print_endline result in
  ()

let generate_random_diagonal () =
  let empty_board =
    [|
      [| 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
      [| 0; 0; 0; 0; 0; 0; 0; 0; 0 |];
    |]
  in
  for b = 1 to 3 do
    let _ =
      fill_box_with_lst empty_board b b (shuffle [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ])
    in
    ()
  done;
  print_board empty_board;
  empty_board

let get_row board (rownum : int) : int array = board.(rownum - 1)

let get_col board (colnum : int) : int array =
  Array.init (Array.length board) (fun row -> board.(row).(colnum - 1))

let get_block board ((blockrow, blockcol) : int * int) : int array =
  let grid_edge = Array.length board in
  let block_edge = int_of_float (sqrt (float_of_int grid_edge)) in
  let result =
    Array.init grid_edge (fun cell ->
        let row_value = ((blockrow - 1) * block_edge) + (cell / block_edge) in
        let col_value = ((blockcol - 1) * block_edge) + (cell mod block_edge) in
        board.(row_value).(col_value))
  in
  (* let _ = print_endline (List.fold_left (fun acc x -> acc ^ string_of_int x)
     "" (Array.to_list result)) in *)
  result

let valid_place board row col num =
  let _ =
    print_endline
      (string_of_bool
         (List.mem num
            (Array.to_list (get_block board (1 + (row / 3), 1 + (col / 3))))))
  in
  let _ =
    print_endline
      (string_of_bool (List.mem num (Array.to_list (get_col board (col + 1)))))
  in
  let _ =
    print_endline
      (string_of_bool (List.mem num (Array.to_list (get_row board (row + 1)))))
  in
  not
    (List.mem num
       (Array.to_list (get_block board (1 + (row / 3), 1 + (col / 3))))
    || List.mem num (Array.to_list (get_col board (col + 1)))
    || List.mem num (Array.to_list (get_row board (row + 1))))

let board_filled board =
  let filled = ref true in
  let pos = 0 in
  while pos < 81 && !filled do
    filled := board.(pos / 9).(pos mod 9) <> 0
  done;
  !filled

let generate_board () =
  let board = generate_random_diagonal () in
  ();

  board
