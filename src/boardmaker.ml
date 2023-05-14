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
  options := List.filter (fun x -> not (Array.mem x block)) !options;
  !options

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
      (* print_endline ("min: " ^ string_of_int min_val ^ " length val: " ^
         string_of_int lengths.(row).(col)); *)
      if lengths.(row).(col) = min_val then
        min_pos_list := (row + 1, col + 1) :: !min_pos_list
    done
  done;
  !min_pos_list

let remove_random_positions board difficulty =
  for i = 0 to (Random.bits () mod 10) + (difficulty * 10) do
    let row = Random.bits () mod 9 in
    let col = Random.bits () mod 9 in
    board.(row).(col) <- i - i
  done;
  board

exception InfiniteLoop

let generate_board_h () =
  let main_board = State.deep_copy_board empty_board in
  let inf_loop_count = ref 0 in

  while not (board_filled main_board) do
    let get_random_element l =
      List.nth l
        (Random.self_init ();
         Random.bits () mod List.length l)
    in

    let row, col =
      let pos_list =
        main_board |> generate_board_options |> get_min_board_options
      in
      (* let _ = print_endline (string_of_int (List.length pos_list)) in *)
      List.nth pos_list
        (Random.self_init ();
         Random.bits () mod List.length pos_list)
    in

    main_board.(row - 1).(col - 1) <-
      get_random_element (get_options main_board row col);
    inf_loop_count := !inf_loop_count + 1;
    if !inf_loop_count > 1000 then
      let _ = print_endline "Infite Loop Detected!" in
      raise InfiniteLoop
    else print_endline (string_of_int !inf_loop_count)
  done;
  main_board

(* Difficulty is number between 1 and 3. Difficulty 1 removes 10-20 numbers.
   Diffuclty 2 removes 20-30 numbers. Difficulty 3 removes 30-40 numbers. *)
let generate_board difficulty =
  let valid = ref false in
  let filled_board = ref [| [| 0 |] |] in
  while not !valid do
    try
      Random.self_init ();
      filled_board := generate_board_h ();
      valid := true
    with
    | Division_by_zero -> ()
    | InfiniteLoop -> ()
  done;
  let result = remove_random_positions !filled_board difficulty in
  print_endline "Board:";
  print_board (State.init_state !filled_board);
  result
