open OUnit2
open Sudoku
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
    (*place error tests*)
    parse_error_test "malformed place" "place" Malformed;
    parse_error_test "malformed place 2 input" "place 3 2" Malformed;
    parse_error_test "malformed place >3 input" "place 3 2 5 6" Malformed;
    (*delete tests*)
    parse_test "parse command delete 6 7" "delete 6 7"
      (Delete { row = 6; col = 7 });
    parse_test "parse command delete 6   7" "delete 6   7"
      (Delete { row = 6; col = 7 });
    (*delete error tests*)
    parse_error_test "malformed delete" "delete" Malformed;
    parse_error_test "malformed delete 1 input" "delete 5" Malformed;
    parse_error_test "malformed delete >2 input" "delete 8 9 1" Malformed;
    (*Solve tests*)
    parse_test "solve" "solve" Solve;
    (*Solve error tests *)
    parse_error_test "malformed solve" "solve 1" Malformed;
    (*Hint tests*)
    parse_test "hint" "hint" Hint;
    (*Hint error tests *)
    parse_error_test "malformed hint" "hint 1" Malformed;
    (*Quit tests*)
    parse_test "quit" "quit" Quit;
    (*Quit error tests *)
    parse_error_test "malformed quit" "quit 1" Malformed;
  ]

(*****************************************************************)
(* State Tests *)
(*****************************************************************)

let deep_copy_board board =
  let new_board = Array.init 9 (fun _ -> Array.init 9 (fun _ -> 0)) in
  for i = 0 to 8 do
    for j = 0 to 8 do
      new_board.(i).(j) <- board.(i).(j)
    done
  done;
  new_board

let board1_grid =
  [|
    [| 2; 0; 0; 3; 0; 0; 0; 0; 0 |];
    [| 8; 0; 4; 0; 6; 2; 0; 0; 3 |];
    [| 0; 1; 3; 8; 0; 0; 2; 0; 0 |];
    [| 0; 0; 0; 0; 2; 0; 3; 9; 0 |];
    [| 5; 0; 7; 0; 0; 0; 6; 2; 1 |];
    [| 0; 3; 2; 0; 0; 6; 0; 0; 0 |];
    [| 0; 2; 0; 0; 0; 9; 1; 4; 0 |];
    [| 6; 0; 1; 2; 5; 0; 8; 0; 9 |];
    [| 0; 0; 0; 0; 0; 1; 0; 0; 2 |];
  |]

let board1_grid_2_2_1 =
  [|
    [| 2; 0; 0; 3; 0; 0; 0; 0; 0 |];
    [| 8; 1; 4; 0; 6; 2; 0; 0; 3 |];
    [| 0; 1; 3; 8; 0; 0; 2; 0; 0 |];
    [| 0; 0; 0; 0; 2; 0; 3; 9; 0 |];
    [| 5; 0; 7; 0; 0; 0; 6; 2; 1 |];
    [| 0; 3; 2; 0; 0; 6; 0; 0; 0 |];
    [| 0; 2; 0; 0; 0; 9; 1; 4; 0 |];
    [| 6; 0; 1; 2; 5; 0; 8; 0; 9 |];
    [| 0; 0; 0; 0; 0; 1; 0; 0; 2 |];
  |]

let start_board_test name (state : State.t) expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (deep_copy_board (start_board state))

let current_board_test name (state : State.t) expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (deep_copy_board (current_board state))

let get_rc_test name f (state : State.t) (row : int) expected_output : test =
  name >:: fun _ -> assert_equal expected_output (f state row)

let get_bc_test name f (state : State.t) ((row, col) : int * int)
    expected_output : test =
  name >:: fun _ -> assert_equal expected_output (f state (row, col))

let next_grid_test name (state : State.t) (row : int) (col : int) (value : int)
    expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (deep_copy_board (next_grid state row col value))

let next_grid_error_test name (state : State.t) (row : int) (col : int)
    (value : int) expected_error : test =
  name >:: fun _ ->
  assert_raises expected_error (fun () ->
      deep_copy_board (next_grid state row col value))

let state_tests =
  let board1 = init_state board1_grid in
  [
    (*start board tests*)
    start_board_test "board 1 start" board1 board1_grid;
    (*current board tests*)
    current_board_test "board 1 current" board1 board1_grid;
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
    (*next grid error tests*)
    next_grid_error_test "invalid row" board1 12 2 1 (InvalidBox (12, 2));
    next_grid_error_test "invalid col" board1 2 12 1 (InvalidBox (2, 12));
    next_grid_error_test "cell is filled" board1 1 1 1 (InvalidBox (1, 1));
    next_grid_error_test "invalid answer" board1 2 2 10 (InvalidAnswer 10);
  ]

let boardmaker_tests = []

let suite =
  "test suite for A2"
  >::: List.flatten [ command_tests; state_tests; boardmaker_tests ]

let _ = run_test_tt_main suite
