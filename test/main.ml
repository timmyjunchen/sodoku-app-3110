(** Testing plan

    For our Ounit testing, all the features in boardmaker, command, and state
    were tested through the ounit for grid sizes of 4x4, 9x9 (2/3 of the
    playable grids). For 16 x 16 grids (the last playable grid), most of the
    functions were tested except for solve and hint as checking if the solved
    board is a valid win takes a lot longer and we wanted our test suite to run
    under a second.

    For our tests, we used glass box testing for all the modules to test the
    different branches of our functions. We implemented this by testing not only
    the "typical" test cases but also any fringe cases to make sure our
    functions worked for any sudoku board. For example, for commands, we made
    sure to test inputs with normal spacing between words, random
    capitalization, and random spacing to make sure our game works, even if the
    user might have mistyped some of their inputs.

    This methodology demonstrates the correctness of the system because by
    testing the typical and fringe cases, we make sure our system works for any
    random board generated which is important so that the user can always reach
    a completed board. Additionally, by testing for correctness in all our
    functions in all our modules, we can make sure that no errors occur in any
    step our game, whether that is board generation, parsing commands, or
    updating the board/game logic*)

open OUnit2
open Sudoku
open Command
open State
open Boardmaker

(*****************************************************************)
(* Command Tests *)
(*****************************************************************)
let parse_test name input expected_output : test =
  name >:: fun _ -> assert_equal expected_output (parse input)

let parse_error_test name input expected_error : test =
  name >:: fun _ -> assert_raises expected_error (fun () -> parse input)

let command_tests =
  [
    (*empty test*)
    parse_error_test "empty" "" Empty;
    (*place tests*)
    parse_test "parse command place 2 3 5" "place 2 3 5"
      (Move { row = 2; col = 3; value = 5 });
    parse_test "parse command place 2  3 5" "place 2  3 5"
      (Move { row = 2; col = 3; value = 5 });
    parse_test "parse command Place 2  3 5" "Place 2  3 5"
      (Move { row = 2; col = 3; value = 5 });
    parse_test "parse command pLaCe 2  3 5" "pLaCe 2  3 5"
      (Move { row = 2; col = 3; value = 5 });
    (*place error tests*)
    parse_error_test "malformed place" "place" Malformed;
    parse_error_test "malformed place 2 input" "place 3 2" Malformed;
    parse_error_test "malformed Place 2 input" "Place 3 2" Malformed;
    parse_error_test "malformed place >3 input" "place 3 2 5 6" Malformed;
    (*delete tests*)
    parse_test "parse command delete 6 7" "delete 6 7"
      (Delete { row = 6; col = 7 });
    parse_test "parse command delete 6   7" "delete 6   7"
      (Delete { row = 6; col = 7 });
    parse_test "parse command Delete 6 7" "Delete 6 7"
      (Delete { row = 6; col = 7 });
    (*delete error tests*)
    parse_error_test "malformed delete" "delete" Malformed;
    parse_error_test "malformed delete 1 input" "delete 5" Malformed;
    parse_error_test "malformed delete >2 input" "delete 8 9 1" Malformed;
    (*Solve tests*)
    parse_test "solve" "solve" Solve;
    parse_test " solve  " " solve  " Solve;
    parse_test "Solve" "Solve" Solve;
    (*Solve error tests *)
    parse_error_test "malformed solve" "solve 1" Malformed;
    (*Hint tests*)
    parse_test "hint" "hint" Hint;
    parse_test "hint  " "hint  " Hint;
    parse_test "hInt" "hInt" Hint;
    (*Hint error tests *)
    parse_error_test "malformed hint" "hint 1" Malformed;
    (*Quit tests*)
    parse_test "quit" "quit" Quit;
    parse_test " quit  " " quit  " Quit;
    parse_test " QUIT  " " QUIT  " Quit;
    (*Quit error tests *)
    parse_error_test "malformed quit" "quit 1" Malformed;
    (*Solver tests*)
    parse_test "solver" "solver" SolveMode;
    (*Solver error tests *)
    parse_error_test "malformed solver" "solver1" Malformed;
    (*File tests*)
    parse_test "file" "file" FileMode;
    (*File error tests *)
    parse_error_test "malformed file" "file 1" Malformed;
    (*Help tests*)
    parse_test "help" "help" Help;
    (*Help error tests *)
    parse_error_test "malformed help" "help 1" Malformed;
  ]

(*****************************************************************)
(* State Tests *)
(*****************************************************************)

(* 9 x 9 grids*)
let data_dir_prefix9x9 = "grids" ^ Filename.dir_sep ^ "9x9" ^ Filename.dir_sep

let board1_grid =
  Yojson.Basic.from_file (data_dir_prefix9x9 ^ "board1_grid.json") |> from_json

let board1_grid_2_2_1 =
  Yojson.Basic.from_file (data_dir_prefix9x9 ^ "board1_grid_2_2_1.json")
  |> from_json

let board221_grid_4_9_3 =
  Yojson.Basic.from_file (data_dir_prefix9x9 ^ "board221_grid_4_9_3.json")
  |> from_json

let almost_completed_grid =
  Yojson.Basic.from_file (data_dir_prefix9x9 ^ "almost_completed_grid.json")
  |> from_json

let completed_grid =
  Yojson.Basic.from_file (data_dir_prefix9x9 ^ "completed_grid.json")
  |> from_json

let completed_invalid_grid =
  Yojson.Basic.from_file (data_dir_prefix9x9 ^ "completed_invalid_grid.json")
  |> from_json

(* 4 x 4 grids*)
let data_dir_prefix4x4 = "grids" ^ Filename.dir_sep ^ "4x4" ^ Filename.dir_sep

let board1_4x4grid =
  Yojson.Basic.from_file (data_dir_prefix4x4 ^ "board1_4x4grid.json")
  |> from_json

let board423_4x4grid =
  Yojson.Basic.from_file (data_dir_prefix4x4 ^ "board423_4x4grid.json")
  |> from_json

let completed_4x4grid =
  Yojson.Basic.from_file (data_dir_prefix4x4 ^ "completed_4x4grid.json")
  |> from_json

let completed_invalid_4x4grid =
  Yojson.Basic.from_file (data_dir_prefix4x4 ^ "completed_invalid_4x4grid.json")
  |> from_json

let board321_4x4grid =
  Yojson.Basic.from_file (data_dir_prefix4x4 ^ "board321_4x4grid.json")
  |> from_json

let unsolvable_4x4grid =
  Yojson.Basic.from_file (data_dir_prefix4x4 ^ "unsolvable_4x4grid.json")
  |> from_json

(* 16 x 16 grids*)
let data_dir_prefix16x16 =
  "grids" ^ Filename.dir_sep ^ "16x16" ^ Filename.dir_sep

let board1_16x16grid =
  Yojson.Basic.from_file (data_dir_prefix16x16 ^ "board1_16x16grid.json")
  |> from_json

let board15511_16x16grid =
  Yojson.Basic.from_file (data_dir_prefix16x16 ^ "board15511_16x16grid.json")
  |> from_json

let board21313_16x16grid =
  Yojson.Basic.from_file (data_dir_prefix16x16 ^ "board21313_16x16grid.json")
  |> from_json

let completed_16x16grid =
  Yojson.Basic.from_file (data_dir_prefix16x16 ^ "completed_16x16grid.json")
  |> from_json

let completed_invalid_16x16grid =
  Yojson.Basic.from_file
    (data_dir_prefix16x16 ^ "completed_invalid_16x16grid.json")
  |> from_json

let almost_complete_16x16grid =
  Yojson.Basic.from_file
    (data_dir_prefix16x16 ^ "almost_complete_16x16grid.json")
  |> from_json

let unsolvable_16x16grid =
  Yojson.Basic.from_file (data_dir_prefix16x16 ^ "unsolvable_16x16grid.json")
  |> from_json

let start_board_test name (state : State.t) expected_output : test =
  name >:: fun _ -> assert_equal expected_output (start_board state)

let current_board_test name (state : State.t) expected_output : test =
  name >:: fun _ -> assert_equal expected_output (current_board state)

let get_rc_test name f (state : State.t) (rc : int) expected_output : test =
  name >:: fun _ -> assert_equal expected_output (f state rc)

let get_bc_test name f (state : State.t) ((row, col) : int * int)
    expected_output : test =
  name >:: fun _ -> assert_equal expected_output (f state (row, col))

let next_grid_test name (state : State.t) (row : int) (col : int) (value : int)
    expected_output : test =
  let copied_state = deep_copy_state state in
  name >:: fun _ ->
  assert_equal expected_output (next_grid copied_state row col value)

let next_grid_error_test name (state : State.t) (row : int) (col : int)
    (value : int) expected_error : test =
  let copied_state = deep_copy_state state in
  name >:: fun _ ->
  assert_raises expected_error (fun () -> next_grid copied_state row col value)

let answer_board_test name f (row : int) (col : int) (value : int)
    (state : State.t) expected_output : test =
  let new_board =
    let copied_state = deep_copy_state state in
    match answer row col value copied_state with
    | Legal new_state -> f new_state
    | Illegal -> [||] (*sudoku boards cant be empty*)
  in
  name >:: fun _ -> assert_equal expected_output new_board

let check_win_test name (state : State.t) expected_output : test =
  name >:: fun _ -> assert_equal expected_output (check_win state)

let solve_board_test name (state : State.t) expected_output : test =
  let copied_state = deep_copy_state state in
  name >:: fun _ ->
  assert_equal expected_output (check_win (solve_board copied_state))

let solve_board_error_test name (state : State.t) expected_error : test =
  let copied_state = deep_copy_state state in
  name >:: fun _ ->
  assert_raises expected_error (fun () -> solve_board copied_state)

let hint_board_test name f (state : State.t) expected_output : test =
  let new_board =
    let copied_state = deep_copy_state state in
    match board_hint copied_state with
    | Legal new_state -> f new_state
    | Illegal -> [||] (*sudoku boards cant be empty*)
  in
  name >:: fun _ -> assert_equal expected_output new_board

let extract_state (f : string) (row : int) (col : int) (value : int)
    (state : State.t) =
  let copied_state = deep_copy_state state in
  match f with
  | "answer" -> (
      match answer row col value copied_state with
      | Legal new_state -> new_state
      | Illegal ->
          failwith "can't happen"
          (*answer will always be legal when using this helper *))
  | "delete" -> (
      match delete row col copied_state with
      | Legal new_state -> new_state
      | Illegal ->
          failwith "can't happen"
          (*delete will always be legal when using this helper*))
  | "hint" -> (
      match board_hint copied_state with
      | Legal new_state -> new_state
      | Illegal ->
          failwith
            "can't happen" (*hint will always be legal when using this helper*))
  | _ -> failwith "can't happen" (*f will always be one of above strings*)

let state_tests9x9 =
  let board1 = init_state board1_grid in
  let board221 = init_state board1_grid_2_2_1 in
  let board493 = extract_state "answer" 4 9 3 board221 in
  let board221_delete = extract_state "delete" 4 9 0 board493 in
  let board_complete = init_state completed_grid in
  let board_complete_invalid = init_state completed_invalid_grid in
  let board_incomplete = init_state almost_completed_grid in
  let board_incomplete_to_complete =
    extract_state "answer" 1 1 5 board_incomplete
  in
  let board_invalid_win = extract_state "answer" 1 1 8 board_incomplete in
  let board1_hint = extract_state "hint" 0 0 0 board1 in

  [
    (*start board tests*)
    start_board_test "board 1 start" board1 board1_grid;
    start_board_test "board 221 start" board221 board1_grid_2_2_1;
    start_board_test "board_incomplete start after answer"
      board_incomplete_to_complete almost_completed_grid;
    start_board_test "board_invalid_win start after answer" board_invalid_win
      almost_completed_grid;
    start_board_test "board_1 start after hint" board1_hint board1_grid;
    (*current board tests*)
    current_board_test "board 1 current" board1 board1_grid;
    current_board_test "board 221 current" board221 board1_grid_2_2_1;
    (*row test*)
    get_rc_test "row 1 board1" get_row board1 1 [| 2; 0; 0; 3; 0; 0; 0; 0; 0 |];
    get_rc_test "row 7 board1" get_row board1 7 [| 0; 2; 0; 0; 0; 9; 1; 4; 0 |];
    get_rc_test "row 4 board493 after answer" get_row board493 4
      [| 0; 0; 0; 0; 2; 0; 3; 9; 3 |];
    get_rc_test "row 4 board221 after delete" get_row board221_delete 4
      [| 0; 0; 0; 0; 2; 0; 3; 9; 0 |];
    (*col test*)
    get_rc_test "col 1 board1" get_col board1 1 [| 2; 8; 0; 0; 5; 0; 0; 6; 0 |];
    get_rc_test "col 7 board1" get_col board1 8 [| 0; 0; 0; 9; 2; 0; 4; 0; 0 |];
    get_rc_test "col 9 board493 after answer" get_col board493 9
      [| 0; 3; 0; 3; 1; 0; 0; 9; 2 |];
    get_rc_test "col 9 board221 after delete" get_col board221_delete 9
      [| 0; 3; 0; 0; 1; 0; 0; 9; 2 |];
    (*block test*)
    get_bc_test "block 1 1 board1" get_block board1 (1, 1)
      [| 2; 0; 0; 8; 0; 4; 0; 1; 3 |];
    get_bc_test "block 3 2 board1" get_block board1 (3, 2)
      [| 0; 0; 9; 2; 5; 0; 0; 0; 1 |];
    get_bc_test "block 2 3 board493 after answer" get_block board493 (2, 3)
      [| 3; 9; 3; 6; 2; 1; 0; 0; 0 |];
    get_bc_test "block 2 3 board221 after delete" get_block board221_delete
      (2, 3)
      [| 3; 9; 0; 6; 2; 1; 0; 0; 0 |];
    (*cell test*)
    get_bc_test "cell 1 1 board1" get_cell board1 (1, 1) 2;
    get_bc_test "cell 3 8 board1" get_cell board1 (3, 8) 0;
    get_bc_test "cell 4 9 board493 after answer" get_cell board493 (4, 9) 3;
    (*next grid test*)
    next_grid_test "put 1 in row 2 col 2" board1 2 2 1 board1_grid_2_2_1;
    next_grid_test "put 3 in row 4 col 9" board221 4 9 3 board221_grid_4_9_3;
    (*next grid error tests*)
    next_grid_error_test "invalid row" board1 12 2 1 (InvalidBox (12, 2));
    next_grid_error_test "invalid col" board1 2 12 1 (InvalidBox (2, 12));
    next_grid_error_test "cell is filled" board1 1 1 1 (InvalidBox (1, 1));
    next_grid_error_test "invalid answer" board1 2 2 10 (InvalidAnswer 10);
    (*current grid answer legal test*)
    answer_board_test "current board1 answer 1 in row 2 col 2" current_board 2 2
      1 board1 board1_grid_2_2_1;
    answer_board_test "current board221 answer 3 in row 4 col 9" current_board 4
      9 3 board221 board221_grid_4_9_3;
    (*current grid answer illegal test*)
    answer_board_test "current board1 answer 1 in row 12 col 2" current_board 12
      2 1 board1 [||];
    answer_board_test "current board1 answer 1 in row 2 col 12" current_board 2
      12 1 board1 [||];
    answer_board_test "current board1 answer 10 in row 2 col 2" current_board 2
      2 10 board1 [||];
    answer_board_test "current board1 answer 1 in row 1 col 1" current_board 1 1
      1 board1 [||];
    answer_board_test "current board221 answer 9 in row 2 col 2" current_board 2
      2 9 board221 [||];
    (*start grid answer legal test*)
    answer_board_test "start board1 answer 1 in row 2 col 2" start_board 2 2 1
      board1 board1_grid;
    answer_board_test "start board221 answer 3 in row 4 col 9" start_board 4 9 3
      board221 board1_grid_2_2_1;
    (*start grid answer illegal test*)
    answer_board_test "start board1 answer 1 in row 12 col 2" start_board 12 2 1
      board1 [||];
    answer_board_test "start board1 answer 1 in row 2 col 12" start_board 2 12 1
      board1 [||];
    answer_board_test "start board1 answer 10 in row 2 col 2" start_board 2 2 10
      board1 [||];
    answer_board_test "start board1 answer 1 in row 1 col 1" start_board 1 1 1
      board1 [||];
    answer_board_test "start board221 answer 9 in row 2 col 2" start_board 2 2 9
      board221 [||];
    (*check win tests*)
    check_win_test "completed board check win" board_complete true;
    check_win_test "completed board check win after answer"
      board_incomplete_to_complete true;
    check_win_test "completed invalid board check win" board_complete_invalid
      false;
    check_win_test "completed invalid board check win after answer"
      board_invalid_win false;
    check_win_test "board1 check win" board1 false;
    (*solve tests*)
    solve_board_test "board1 check solve" board1 true;
    solve_board_test "board_incomplete check solve" board_incomplete true;
    (*solve error tests*)
    solve_board_error_test "board221 check not solvable" board221
      UnsolvableBoard;
    (*hint legal test*)
    solve_board_test "board1 hint" board1_hint true;
    (*start grid hint legal tests*)
    hint_board_test "start board1 hint" start_board board1 board1_grid;
    (*start grid hint illegal tests*)
    hint_board_test "start board221 hint" start_board board221 [||];
  ]

let delete_board_test name f (row : int) (col : int) (state : State.t)
    expected_output : test =
  let copied_state = deep_copy_state state in
  let new_board =
    match delete row col copied_state with
    | Legal new_state -> f new_state
    | Illegal -> [||] (*sudoku boards cant be empty*)
  in
  name >:: fun _ -> assert_equal expected_output new_board

let delete_tests9x9 =
  let board1 = init_state board1_grid in
  let board221 = extract_state "answer" 2 2 1 board1 in
  let board493 = extract_state "answer" 4 9 3 board221 in

  [
    (*current grid delete legal test*)
    delete_board_test "current delete board221 row 2 col 2" current_board 2 2
      board221 board1_grid;
    delete_board_test "current delete board493 row 4 col 9" current_board 4 9
      board493 board1_grid_2_2_1;
    (*current grid delete illegal test*)
    delete_board_test "current board1 delete in row 12 col 2" current_board 12 2
      board1 [||];
    delete_board_test "current board1 delete in row 2 col 12" current_board 2 12
      board1 [||];
    delete_board_test "current board1 delete in row 1 col 1" current_board 1 1
      board1 [||];
    delete_board_test "current board221 delete in row 8 col 8" current_board 8 8
      board221 [||];
    (*start grid delete legal test*)
    delete_board_test "start delete board221 row 2 col 2" start_board 2 2
      board221 board1_grid;
    delete_board_test "start delete board493 row 4 col 9" start_board 4 9
      board493 board1_grid;
    (*start grid delete illegal test*)
    delete_board_test "start board1 delete in row 12 col 2" start_board 12 2
      board1 [||];
    delete_board_test "start board1 delete in row 2 col 12" start_board 2 12
      board1 [||];
    delete_board_test "start board1 delete in row 1 col 1" start_board 1 1
      board1 [||];
    delete_board_test "start board221 delete in row 8 col 8" start_board 8 8
      board221 [||];
  ]

let state_tests4x4 =
  let board1 = init_state board1_4x4grid in
  let board423 = init_state board423_4x4grid in
  let board321 = extract_state "answer" 3 2 1 board423 in
  let board_complete = init_state completed_4x4grid in
  let board_invalid = init_state completed_invalid_4x4grid in
  let board1_hint = extract_state "hint" 0 0 0 board1 in
  let unsolvable = init_state unsolvable_4x4grid in
  [
    (*start board tests*)
    start_board_test "board 1 start" board1 board1_4x4grid;
    start_board_test "board 423 start" board423 board423_4x4grid;
    start_board_test "board 321 start after answer" board321 board423_4x4grid;
    (*current board tests*)
    current_board_test "board 1 current" board1 board1_4x4grid;
    current_board_test "board 423 current" board423 board423_4x4grid;
    (*row test*)
    get_rc_test "board 1 get row 1" get_row board1 1 [| 1; 4; 0; 0 |];
    get_rc_test "board 1 get row 3" get_row board1 3 [| 4; 0; 0; 3 |];
    get_rc_test "board 423 get row 4" get_row board423 4 [| 0; 3; 4; 1 |];
    get_rc_test "board 321 get row 3 after answer" get_row board321 3
      [| 4; 1; 0; 3 |];
    (*col test*)
    get_rc_test "board 1 get col 1" get_col board1 1 [| 1; 0; 4; 0 |];
    get_rc_test "board 1 get col 3" get_col board1 3 [| 0; 1; 0; 4 |];
    get_rc_test "board 423 get col 2" get_col board423 2 [| 4; 2; 0; 3 |];
    get_rc_test "board 321 get col 2 after answer" get_col board321 2
      [| 4; 2; 1; 3 |];
    (*block test*)
    get_bc_test "board 1 get block 1 1" get_block board1 (1, 1) [| 1; 4; 0; 2 |];
    get_bc_test "board 1 get block 2 2" get_block board1 (2, 2) [| 0; 3; 4; 1 |];
    get_bc_test "board 423 get block 2 1" get_block board423 (2, 1)
      [| 4; 0; 0; 3 |];
    get_bc_test "board 321 get block 2 1 after answer" get_block board321 (2, 1)
      [| 4; 1; 0; 3 |];
    (*cell test*)
    get_bc_test "board 1 get cell 1 1" get_cell board1 (1, 1) 1;
    get_bc_test "board 1 get cell 3 4" get_cell board1 (3, 4) 3;
    get_bc_test "board 423 get cell 4 2" get_cell board423 (4, 2) 3;
    get_bc_test "board 321 get block 3 2 after answer" get_cell board321 (3, 2)
      1;
    (*next grid test*)
    next_grid_test "put 3 in row 4 col 2" board1 4 2 3 board423_4x4grid;
    next_grid_test "put 1 in row 3 col 2" board423 3 2 1 board321_4x4grid;
    (*next grid error test*)
    next_grid_error_test "invalid row" board1 5 2 1 (InvalidBox (5, 2));
    next_grid_error_test "invalid col" board1 2 5 1 (InvalidBox (2, 5));
    next_grid_error_test "cell is filled" board1 1 1 1 (InvalidBox (1, 1));
    next_grid_error_test "invalid answer" board1 2 2 10 (InvalidAnswer 10);
    (*current grid answer legal test*)
    answer_board_test "current board1 answer 3 in row 4 col 2" current_board 4 2
      3 board1 board423_4x4grid;
    answer_board_test "current board423 answer 1 in row 3 col 2" current_board 3
      2 1 board423 board321_4x4grid;
    (*current grid answer illegal test*)
    answer_board_test "current board1 answer 1 in row 5 col 2" current_board 5 2
      1 board1 [||];
    answer_board_test "current board1 answer 1 in row 2 col 6" current_board 2 6
      1 board1 [||];
    answer_board_test "current board1 answer 10 in row 2 col 2" current_board 2
      2 10 board1 [||];
    answer_board_test "current board1 answer 1 in row 1 col 1" current_board 1 1
      1 board1 [||];
    answer_board_test "current board423 answer 9 in row 4 col 2" current_board 4
      2 9 board423 [||];
    (*start grid answer legal test*)
    answer_board_test "start board1 answer 3 in row 4 col 2" start_board 4 2 3
      board1 board1_4x4grid;
    (*start grid answer illegal test*)
    answer_board_test "start board1 answer 1 in row 5 col 2" start_board 5 2 1
      board1 [||];
    answer_board_test "start board1 answer 1 in row 2 col 6" start_board 2 6 1
      board1 [||];
    answer_board_test "start board1 answer 10 in row 2 col 2" start_board 2 2 10
      board1 [||];
    answer_board_test "start board1 answer 1 in row 1 col 1" start_board 1 1 1
      board1 [||];
    answer_board_test "start board423 answer 9 in row 4 col 2" start_board 4 2 9
      board423 [||];
    (*check win test*)
    check_win_test "completed board check win" board_complete true;
    check_win_test "completed invalid board check win" board_invalid false;
    (*solve tests*)
    solve_board_test "board1 check solve" board1 true;
    solve_board_test "board423 check solve" board423 true;
    solve_board_test "board321 check solve" board321 true;
    (*solve error tests*)
    solve_board_error_test "unsolvable check not solvable" unsolvable
      UnsolvableBoard;
    (*hint legal test*)
    solve_board_test "board1 hint" board1_hint true;
    (*start grid hint legal tests*)
    hint_board_test "start board1 hint" start_board board1 board1_4x4grid;
    (*start grid hint illegal tests*)
    hint_board_test "start unsolvable hint" start_board unsolvable [||];
  ]

let delete_tests4x4 =
  let board1 = init_state board1_4x4grid in
  let board423 = extract_state "answer" 4 2 3 board1 in
  let board321 = extract_state "answer" 3 2 1 board423 in

  [
    (*current grid delete legal test*)
    delete_board_test "current delete board423 row 4 col 2" current_board 4 2
      board423 board1_4x4grid;
    delete_board_test "current delete board321 row 3 col 2" current_board 3 2
      board321 board423_4x4grid;
    (*current grid delete illegal test*)
    delete_board_test "current board1 delete in row 6 col 2" current_board 6 2
      board1 [||];
    delete_board_test "current board1 delete in row 2 col 6" current_board 2 6
      board1 [||];
    delete_board_test "current board1 delete in row 1 col 1" current_board 1 1
      board1 [||];
    delete_board_test "current board423 delete in row 2 col 2" current_board 2 2
      board423 [||];
    (*start grid delete legal test*)
    delete_board_test "start delete board423 row 4 col 2" start_board 4 2
      board423 board1_4x4grid;
    delete_board_test "start delete board321 row 3 col 2" start_board 3 2
      board321 board1_4x4grid;
    (*start grid delete illegal test*)
    delete_board_test "start board1 delete in row 5 col 2" start_board 5 2
      board1 [||];
    delete_board_test "start board1 delete in row 2 col 5" start_board 2 5
      board1 [||];
    delete_board_test "start board1 delete in row 1 col 1" start_board 1 1
      board1 [||];
    delete_board_test "start board423 delete in row 2 col 2" start_board 2 2
      board423 [||];
  ]

let state_tests16x16 =
  let board1 = init_state board1_16x16grid in
  let board15511 = init_state board15511_16x16grid in
  let board21313 = extract_state "answer" 2 13 13 board15511 in
  let board_complete = init_state completed_16x16grid in
  let board_invalid = init_state completed_invalid_16x16grid in
  let almost_complete = init_state almost_complete_16x16grid in
  let almost_complete_hint = extract_state "hint" 0 0 0 almost_complete in
  let unsolvable = init_state unsolvable_16x16grid in
  [
    (*start tests*)
    start_board_test "board 1 start" board1 board1_16x16grid;
    start_board_test "board 15511 start" board15511 board15511_16x16grid;
    start_board_test "board 21313 start after answer" board21313
      board15511_16x16grid;
    (*current tests*)
    current_board_test "board 1 current" board1 board1_16x16grid;
    current_board_test "board 15511 current" board15511 board15511_16x16grid;
    (*row test*)
    get_rc_test "board 1 get row 12" get_row board1 12
      [| 0; 0; 13; 8; 12; 14; 4; 0; 0; 0; 16; 0; 3; 0; 0; 0 |];
    get_rc_test "board 21313 get row 2" get_row board21313 2
      [| 5; 9; 4; 1; 8; 0; 0; 0; 0; 0; 6; 15; 13; 2; 3; 11 |];
    (*col test*)
    get_rc_test "board 1 get col 3" get_col board1 3
      [| 0; 4; 8; 0; 0; 14; 0; 0; 0; 10; 5; 13; 9; 3; 7; 0 |];
    get_rc_test "board 21313 get col 13" get_col board21313 13
      [| 7; 13; 0; 1; 0; 0; 0; 0; 5; 4; 14; 3; 6; 0; 0; 0 |];
    (*block test*)
    get_bc_test "board 1 get block 3 4" get_block board1 (3, 4)
      [| 5; 0; 0; 6; 4; 0; 0; 2; 14; 16; 0; 0; 3; 0; 0; 0 |];
    get_bc_test "board 21313 get block 1 4" get_block board21313 (1, 4)
      [| 7; 6; 0; 0; 13; 2; 3; 11; 0; 0; 0; 16; 1; 8; 0; 9 |];
    (*cell test*)
    get_bc_test "board 1 get cell 11 14" get_cell board1 (11, 14) 16;
    get_bc_test "board 21313 get cell 2 13" get_cell board21313 (2, 13) 13;
    (*next grid test*)
    next_grid_test "put 11 in row 15 col 5" board1 15 5 11 board15511_16x16grid;
    next_grid_test "put 13 in row 2 col 13" board15511 2 13 13
      board21313_16x16grid;
    (*next grid error test*)
    next_grid_error_test "invalid row" board1 25 2 1 (InvalidBox (25, 2));
    next_grid_error_test "invalid col" board1 2 17 1 (InvalidBox (2, 17));
    next_grid_error_test "cell is filled" board1 1 1 1 (InvalidBox (1, 1));
    next_grid_error_test "invalid answer" board1 2 6 17 (InvalidAnswer 17);
    (*current grid answer legal test*)
    answer_board_test "current board1 answer 11 in row 15 col 5" current_board
      15 5 11 board1 board15511_16x16grid;
    (*current grid answer illegal test*)
    answer_board_test "current board1 answer 1 in row 17 col 2" current_board 17
      2 1 board1 [||];
    answer_board_test "current board1 answer 1 in row 2 col 26" current_board 2
      26 1 board1 [||];
    answer_board_test "current board1 answer 100 in row 2 col 2" current_board 2
      2 100 board1 [||];
    answer_board_test "current board1 answer 1 in row 1 col 1" current_board 1 1
      1 board1 [||];
    answer_board_test "current board423 answer 9 in row 4 col 5" current_board 4
      5 9 board15511 [||];
    (*start grid answer legal test*)
    answer_board_test "start board1 answer 11 in row 15 col 5" start_board 15 5
      11 board1 board1_16x16grid;
    (*start grid answer illegal test*)
    answer_board_test "start board1 answer 1 in row 17 col 2" start_board 17 2 1
      board1 [||];
    answer_board_test "start board1 answer 1 in row 2 col 26" start_board 2 26 1
      board1 [||];
    answer_board_test "start board1 answer 100 in row 2 col 2" start_board 2 2
      100 board1 [||];
    answer_board_test "start board1 answer 1 in row 1 col 1" start_board 1 1 1
      board1 [||];
    answer_board_test "start board423 answer 9 in row 4 col 5" start_board 4 5 9
      board15511 [||];
    (*check win test*)
    check_win_test "completed board check win" board_complete true;
    (*check win test*)
    check_win_test "completed invalid board check win" board_invalid false;
    (*solve tests*)
    solve_board_test "16x16 check solve" almost_complete true;
    (*solve error tests*)
    solve_board_error_test "unsolvable check not solvable" unsolvable
      UnsolvableBoard;
    (*hint legal test*)
    solve_board_test "almost complete hint" almost_complete_hint true;
    (*start grid hint legal tests*)
    hint_board_test "start almost complete hint" start_board almost_complete
      almost_complete_16x16grid;
    (*start grid hint illegal tests*)
    hint_board_test "start unsolvable hint" start_board unsolvable [||];
  ]

let delete_tests16x16 =
  let board1 = init_state board1_16x16grid in
  let board15511 = extract_state "answer" 15 5 11 board1 in
  let board21313 = extract_state "answer" 2 13 13 board15511 in

  [
    (*current grid delete legal test*)
    delete_board_test "current delete board15511 row 15 col 5" current_board 15
      5 board15511 board1_16x16grid;
    delete_board_test "current delete board21313 row 2 col 13" current_board 2
      13 board21313 board15511_16x16grid;
    (*current grid delete illegal test*)
    delete_board_test "current board1 delete in row 17 col 2" current_board 17 2
      board1 [||];
    delete_board_test "current board1 delete in row 2 col 17" current_board 2 17
      board1 [||];
    delete_board_test "current board1 delete in row 1 col 1" current_board 1 1
      board1 [||];
    delete_board_test "current board15511 delete in row 3 col 3" current_board 3
      3 board15511 [||];
    (*start grid delete legal test*)
    delete_board_test "start delete board15511 row 15 col 5" start_board 15 5
      board15511 board1_16x16grid;
    delete_board_test "start delete board21313 row 2 col 13" start_board 2 13
      board21313 board1_16x16grid;
    (*start grid delete illegal test*)
    delete_board_test "start board1 delete in row -4 col 2" start_board (-4) 2
      board1 [||];
    delete_board_test "start board1 delete in row 2 col 0" start_board 2 0
      board1 [||];
    delete_board_test "start board1 delete in row 1 col 1" start_board 1 1
      board1 [||];
    delete_board_test "start board15511 delete in row 3 col 3" start_board 3 3
      board15511 [||];
  ]

(*****************************************************************)
(* Board Generation Tests *)
(*****************************************************************)

let data_dir_prefix_generation =
  "grids" ^ Filename.dir_sep ^ "board_generation" ^ Filename.dir_sep

let test_board =
  Yojson.Basic.from_file (data_dir_prefix_generation ^ "test_board.json")
  |> from_json

let get_options_test name f (board : int array array) (row : int) (col : int)
    expected_output : test =
  name >:: fun _ -> assert_equal expected_output (f board row col)

let generate_board_test name (dimensions : int) (difficulty : int)
    expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output ([||] <> generate_board dimensions difficulty)

let generate_board_solve_test name (dimensions : int) (difficulty : int)
    expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (check_win
       (solve_board (init_state (generate_board dimensions difficulty))))

let generate_board_not_solved_test name (dimensions : int) (difficulty : int)
    expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (check_win (init_state (generate_board dimensions difficulty)))

let generate_board_hint_test name (dimensions : int) (difficulty : int)
    expected_output : test =
  name >:: fun _ ->
  let generated_board = init_state (generate_board dimensions difficulty) in
  let hint_board = extract_state "hint" 0 0 0 generated_board in
  assert_equal expected_output (check_win (solve_board hint_board))

let boardmaker_tests =
  [
    (*get options tests*)
    get_options_test "get options test_board 4 1" get_options test_board 4 1
      [ 3; 6; 7; 9 ];
    get_options_test "get options test_board 4 3" get_options test_board 4 3
      [ 4; 5; 7; 9 ];
    (*generate board tests not empty*)
    generate_board_test "board generate test 4 1" 4 1 true;
    generate_board_test "board generate test 4 3" 4 3 true;
    generate_board_test "board generate test 4 5" 4 5 true;
    generate_board_test "board generate test 9 1" 9 1 true;
    generate_board_test "board generate test 9 3" 9 3 true;
    generate_board_test "board generate test 9 5" 9 5 true;
    (*generate board tests solvable*)
    generate_board_solve_test "board generate 4 1 solvable" 4 1 true;
    generate_board_solve_test "board generate 4 3 solvable" 4 3 true;
    generate_board_solve_test "board generate 4 5 solvable" 4 5 true;
    generate_board_solve_test "board generate 9 1 solvable" 9 1 true;
    generate_board_solve_test "board generate 9 3 solvable" 9 3 true;
    generate_board_solve_test "board generate 9 5 solvable" 9 5 true;
    (*generate board tests not solved*)
    generate_board_not_solved_test "board generate 4 1 solved" 4 1 false;
    generate_board_not_solved_test "board generate 4 3 solved" 4 3 false;
    generate_board_not_solved_test "board generate 4 5 solved" 4 5 false;
    generate_board_not_solved_test "board generate 9 1 solved" 9 1 false;
    generate_board_not_solved_test "board generate 9 3 solved" 9 3 false;
    generate_board_not_solved_test "board generate 9 5 solved" 9 5 false
    (*generate board tests hint*);
    generate_board_hint_test "board generate 4 1 hint" 4 1 true;
    generate_board_hint_test "board generate 4 3 hint" 4 3 true;
    generate_board_hint_test "board generate 4 5 hint" 4 5 true;
    generate_board_hint_test "board generate 9 1 hint" 9 1 true;
    generate_board_hint_test "board generate 9 3 hint" 9 3 true;
    generate_board_hint_test "board generate 9 5 hint" 9 5 true;
  ]

let suite =
  "test suite for A2"
  >::: List.flatten
         [
           command_tests;
           state_tests9x9;
           state_tests4x4;
           delete_tests9x9;
           delete_tests4x4;
           delete_tests16x16;
           state_tests16x16;
           boardmaker_tests;
         ]

let _ = run_test_tt_main suite
