(* let rec print_list lst = match lst with | [] -> () | h :: t -> let _ =
   print_int h in print_list t *)

let shuffle lst =
  Random.self_init ();
  let assoc = List.map (fun e -> (Random.int 10000000, e)) lst in
  let shuffled =
    List.sort (fun e1 e2 -> if fst e1 < fst e2 then -1 else 1) assoc
  in
  let _ =
    print_endline
      (List.fold_left
         (fun acc x ->
           acc ^ " " ^ string_of_int (fst x) ^ "," ^ string_of_int (snd x))
         "" shuffled)
  in
  let result = List.map (fun e -> snd e) shuffled in
  result
(* let shuffle d = Random.self_init (); let nd = List.map (fun c -> (Random.bits
   (), c)) d in let sond = List.sort compare nd in List.map snd sond *)

let fill_box_with_lst board box_row box_col lst =
  print_endline (List.fold_left (fun acc x -> acc ^ string_of_int x) "" lst);
  for row = (box_row - 1) * 3 to ((box_row - 1) * 3) + 2 do
    for col = (box_col - 1) * 3 to ((box_col - 1) * 3) + 2 do
      board.(row).(col) <- List.nth lst ((row mod 3 * 3) + (col mod 3))
    done
  done;
  board

(* let random_9_lst = shuffle [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ] *)

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
    (* let _ = Random.self_init () in *)
    let _ =
      fill_box_with_lst empty_board b b (shuffle [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ])
    in
    ()
  done;
  print_endline "DIAGONALIZED BOARD";
  print_board empty_board;
  empty_board
