open State

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
  empty_board

let valid_place board row col num =
  let board = State.init_state board in
  (* let _ = print_endline (string_of_bool (List.mem num (Array.to_list
     (get_block board (1 + (row / 3), 1 + (col / 3)))))) in let _ =
     print_endline (string_of_bool (List.mem num (Array.to_list (get_col board
     (col + 1))))) in let _ = print_endline (string_of_bool (List.mem num
     (Array.to_list (get_row board (row + 1))))) in *)
  not
    (List.mem num
       (Array.to_list
          (get_block board (1 + ((row - 1) / 3), 1 + ((col - 1) / 3))))
    || List.mem num (Array.to_list (get_col board col))
    || List.mem num (Array.to_list (get_row board row)))

let board_filled board =
  let filled = ref true in
  let pos = ref 0 in
  while !pos < Array.length board * Array.length board.(0) && !filled do
    filled := board.(!pos / 9).(!pos mod 9) <> 0;
    pos := !pos + 1
  done;
  !filled

let get_options board row_num col_num =
  let board = State.init_state board in
  let options = ref [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ] in
  let row = get_row board row_num in
  options := List.filter (fun x -> not (Array.mem x row)) !options;
  let col = get_col board col_num in
  options := List.filter (fun x -> not (Array.mem x col)) !options;
  let block =
    get_block board (1 + ((row_num - 1) / 3), 1 + ((col_num - 1) / 3))
  in
  (* let _ = print_endline (Array.fold_left (fun acc_l l -> acc_l ^
     string_of_int l) "block: " block) in *)
  options := List.filter (fun x -> not (Array.mem x block)) !options;
  (* print_endline (List.fold_left (fun acc_l l -> acc_l ^ string_of_int l)
     "OPTIONS: " !options); *)
  !options

let get_block_options board row_num col_num =
  let block_options = Array.make 9 [] in
  for row_i = (row_num * 3) - 2 to row_num * 3 do
    for col_i = (col_num * 3) - 2 to col_num * 3 do
      block_options.(((row_i - 1) mod 3 * 3) + ((col_i - 1) mod 3)) <-
        get_options board row_i col_i
    done
  done;
  block_options

(* creates a board where each position has a list of the possible options *)
let generate_board_options board =
  let board_options = Array.make_matrix 9 9 [] in
  for row = 0 to Array.length board - 1 do
    for col = 0 to Array.length board.(0) - 1 do
      if board.(row).(col) = 0 then
        board_options.(row).(col) <- get_options board (row + 1) (col + 1)
    done
  done;
  board_options

(* let rec get_min_list (lst : int list) : int = match lst with | [] -> 0 | [ h
   ] -> h | h :: t -> if h < get_min_list t then h else get_min_list t *)

(* min can not be 0 *)
let get_min_matrix arr =
  let min_val = ref 10 in
  for row = 0 to Array.length arr - 1 do
    for col = 0 to Array.length arr.(0) - 1 do
      if arr.(row).(col) <> 0 && arr.(row).(col) < !min_val then
        min_val := arr.(row).(col)
    done
  done;
  !min_val

let get_min_board_options (board_options : int list array array) =
  let lengths =
    Array.map
      (fun row -> Array.map (fun lst -> List.length lst) row)
      board_options
  in
  let min_val = get_min_matrix lengths in
  let min_pos_list = ref [] in
  for row = 0 to Array.length lengths - 1 do
    for col = 0 to Array.length lengths.(0) - 1 do
      print_endline
        ("min: " ^ string_of_int min_val ^ "   length val: "
        ^ string_of_int lengths.(row).(col));
      if lengths.(row).(col) = min_val then
        min_pos_list := (row + 1, col + 1) :: !min_pos_list
    done
  done;
  !min_pos_list

(* let open_positions board = let lst = ref [] in for row = 0 to Array.length
   board - 1 do for col = 0 to Array.length board.(0) - 1 do if
   board.(row).(col) <> 0 then lst := (row + 1, col + 1) :: !lst done done;
   !lst *)

let generate_board_h () =
  let main_board = empty_board in

  while not (board_filled main_board) do
    let get_random_element l =
      List.nth l
        (Random.self_init ();
         Random.bits () mod List.length l)
    in

    let row, col =
      (* main_board |> generate_board_options |> get_min_board_options |>
         get_random_element *)
      let pos_list =
        main_board |> generate_board_options |> get_min_board_options
      in
      let _ = print_endline (string_of_int (List.length pos_list)) in
      List.nth pos_list
        (Random.self_init ();
         Random.bits () mod List.length pos_list)
    in
    main_board.(row - 1).(col - 1) <-
      get_random_element (get_options main_board row col);
    print_endline "----------------";
    print_board (State.init_state main_board);
    print_endline (string_of_bool (not (board_filled main_board)))
  done;
  main_board

let generate_board () =
  let valid = ref false in
  let result = ref [| [| 0 |] |] in
  while not !valid do
    try
      result := generate_board_h ();
      valid := true
    with Division_by_zero -> ()
  done;
  !result

(* let generate_block board row_num col_num = let block = Array.make 9 0 in let
   counter = ref 0 in let row_i = ref ((row_num * 3) - 2) in let col_i = ref
   ((col_num * 3) - 2) in while !row_i <= row_num * 3 && !counter < 20 do col_i
   := (col_num * 3) - 2; while !col_i <= col_num * 3 && !counter < 20 do counter
   := 0; let options = get_block_options board row_num col_num in let _ =
   print_endline (Array.fold_left (fun acc x -> acc ^ "\n" ^ List.fold_left (fun
   acc_l l -> acc_l ^ string_of_int l) "" x) "options: " options) in let num =
   ref (get_random_element options.(((!row_i - 1) mod 3 * 3) + ((!col_i - 1) mod
   3))) in

   while (not (valid_place board !row_i !col_i !num)) && !counter < 20 do if
   List.length options.(((!row_i - 1) mod 3 * 3) + ((!col_i - 1) mod 3)) = 0
   then counter := 20 else num := get_random_element options.(((!row_i - 1) mod
   3 * 3) + ((!col_i - 1) mod 3)); print_endline ("LikelyFill: " ^ string_of_int
   !num ^ string_of_bool (valid_place board !row_i !col_i !num) ^ " " ^
   string_of_int !row_i ^ " " ^ string_of_int !col_i); counter := !counter + 1
   done; block.(((!row_i - 1) mod 3 * 3) + ((!col_i - 1) mod 3)) <- !num; let _
   = fill_box_with_lst board row_num col_num (Array.to_list block) in
   print_board board; col_i := !col_i + 1 done; row_i := !row_i + 1 done;
   print_endline (Array.fold_left (fun acc_l l -> acc_l ^ string_of_int l)
   "block: " block); if !counter >= 20 then let _ = fill_box_with_lst board
   row_num col_num [ 0; 0; 0; 0; 0; 0; 0; 0; 0 ] in None else Some block

   let generate_board () = let board = ref (generate_random_diagonal ()) in ();

   !board *)
