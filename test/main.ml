open OUnit2
open Sudoku
open Grid
open Command
open State

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
  ]

(*****************************************************************)
(* State Tests *)
(*****************************************************************)

let data_dir_prefix = "grids" ^ Filename.dir_sep

let board1_grid =
  Yojson.Basic.from_file (data_dir_prefix ^ "board1_grid.json") |> from_json

let board1_grid_2_2_1 =
  Yojson.Basic.from_file (data_dir_prefix ^ "board1_grid_2_2_1.json")
  |> from_json

let board221_grid_4_9_3 =
  Yojson.Basic.from_file (data_dir_prefix ^ "board221_grid_4_9_3.json")
  |> from_json

let almost_completed_grid =
  Yojson.Basic.from_file (data_dir_prefix ^ "almost_completed_grid.json")
  |> from_json

let completed_grid =
  Yojson.Basic.from_file (data_dir_prefix ^ "completed_grid.json") |> from_json

let completed_invalid_grid =
  Yojson.Basic.from_file (data_dir_prefix ^ "completed_invalid_grid.json")
  |> from_json

let start_board_test name (state : State.t) expected_output : test =
  name >:: fun _ -> assert_equal expected_output (start_board state)

let current_board_test name (state : State.t) expected_output : test =
  name >:: fun _ -> assert_equal expected_output (current_board state)

let get_rc_test name f (state : State.t) (row : int) expected_output : test =
  name >:: fun _ -> assert_equal expected_output (f state row)

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
          failwith "can't happen"
          (*hint will always be legal when using this helper*))
  | _ -> failwith "can't happen" (*f will always be one of above strings*)

let state_tests =
  let board1 = init_state board1_grid in
  let board221 = init_state board1_grid_2_2_1 in
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
    (*col test*)
    get_rc_test "col 1 board1" get_col board1 1 [| 2; 8; 0; 0; 5; 0; 0; 6; 0 |];
    get_rc_test "row 7 board1" get_col board1 8 [| 0; 0; 0; 9; 2; 0; 4; 0; 0 |];
    (*block test*)
    get_bc_test "block 1 1 board1" get_block board1 (1, 1)
      [| 2; 0; 0; 8; 0; 4; 0; 1; 3 |];
    get_bc_test "block 3 2 board1" get_block board1 (3, 2)
      [| 0; 0; 9; 2; 5; 0; 0; 0; 1 |];
    (*cell test*)
    get_bc_test "cell 1 1 board1" get_cell board1 (1, 1) 2;
    get_bc_test "cell 3 8 board1" get_cell board1 (3, 8) 0;
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
    (*hint tests*)
    solve_board_test "board1 check hint" board1_hint true;
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

let delete_tests =
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

let boardmaker_tests = []

let suite =
  "test suite for A2"
  >::: List.flatten
         [ command_tests; state_tests; delete_tests; boardmaker_tests ]

let _ = run_test_tt_main suite
