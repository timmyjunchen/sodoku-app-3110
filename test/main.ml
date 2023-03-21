open OUnit2
open Sudoku
open Grid
open State
open Command

let board1_grid =
  [
    [ 2; 0; 0; 3; 0; 0; 0; 0; 0 ];
    [ 8; 0; 4; 0; 6; 2; 0; 0; 3 ];
    [ 0; 1; 3; 8; 0; 0; 2; 0; 0 ];
    [ 0; 0; 0; 0; 2; 0; 3; 9; 0 ];
    [ 5; 0; 7; 0; 0; 0; 6; 2; 1 ];
    [ 0; 3; 2; 0; 0; 6; 0; 0; 0 ];
    [ 0; 2; 0; 0; 0; 9; 1; 4; 0 ];
    [ 6; 0; 1; 2; 5; 0; 8; 0; 9 ];
    [ 0; 0; 0; 0; 0; 1; 0; 0; 2 ];
  ]

let board1 = board_setup board1_grid

(*adventure tests*)
let start_board_test name adv expected_output : test =
  name >:: fun _ -> assert_equal expected_output (start_board adv)

let grid_tests =
  [
    (*start board test*)
    start_board_test "board1 start board" board1 board1_grid;
  ]

let command_tests =
  [
    ( {|parse command place 2 3 5|} >:: fun _ ->
      assert_equal (Move { row = 2; col = 3; value = 5 }) (parse "place 2 3 5")
    );
  ]

(*state tests*)
let current_board_test name (state : State.t) expected_output : test =
  name >:: fun _ -> assert_equal expected_output (current_board state)

let next_grid_test name st row col value expected_output : test =
  name >:: fun _ -> assert_equal expected_output (next_grid st row col value)

let next_grid_error_test name st row col value expected_error : test =
  name >:: fun _ ->
  assert_raises expected_error (fun () -> next_grid st row col value)

let board1_grid_2_2_1 =
  [
    [ 2; 0; 0; 3; 0; 0; 0; 0; 0 ];
    [ 8; 1; 4; 0; 6; 2; 0; 0; 3 ];
    [ 0; 1; 3; 8; 0; 0; 2; 0; 0 ];
    [ 0; 0; 0; 0; 2; 0; 3; 9; 0 ];
    [ 5; 0; 7; 0; 0; 0; 6; 2; 1 ];
    [ 0; 3; 2; 0; 0; 6; 0; 0; 0 ];
    [ 0; 2; 0; 0; 0; 9; 1; 4; 0 ];
    [ 6; 0; 1; 2; 5; 0; 8; 0; 9 ];
    [ 0; 0; 0; 0; 0; 1; 0; 0; 2 ];
  ]

let answer_current_board_test name row col value st expected_output : test =
  let new_current_board =
    match answer row col value st with
    | Legal new_state -> current_board new_state
    | Illegal -> [] (*sudoku boards cant be empty*)
  in
  name >:: fun _ -> assert_equal expected_output new_current_board

let state_tests =
  let board1_init = init_state board1 in
  [
    (*current board test*)
    current_board_test "board 1 initial state" board1_init board1_grid;
    ({|print board test|} >:: fun _ -> assert_equal () (print_board board1_init));
    ( {|row test|} >:: fun _ ->
      assert_equal [ 2; 0; 0; 3; 0; 0; 0; 0; 0 ] (get_row board1_init 1) );
    ( {|col test|} >:: fun _ ->
      assert_equal [ 0; 0; 1; 0; 0; 3; 2; 0; 0 ] (get_col board1_init 2) );
    ( {|block test 1|} >:: fun _ ->
      assert_equal [ 2; 0; 0; 8; 0; 4; 0; 1; 3 ] (get_block board1_init (1, 1))
    );
    ( {|block test 2|} >:: fun _ ->
      assert_equal [ 0; 0; 9; 2; 5; 0; 0; 0; 1 ] (get_block board1_init (3, 2))
    );
    ({|cell test 1|} >:: fun _ -> assert_equal 0 (get_cell board1_init (6, 9)));
    ({|cell test 2|} >:: fun _ -> assert_equal 2 (get_cell board1_init (4, 5)));
    (*next grid test*)
    next_grid_test "put 1 in row 2 col 2" board1_init 2 2 1 board1_grid_2_2_1;
    (*next grid error tests*)
    next_grid_error_test "invalid row" board1_init 12 2 1 (InvalidBox (12, 2));
    next_grid_error_test "invalid col" board1_init 2 12 1 (InvalidBox (2, 12));
    next_grid_error_test "cell is filled" board1_init 1 1 1 (InvalidBox (1, 1));
    next_grid_error_test "invalid answer" board1_init 2 2 10 (InvalidAnswer 10);
    (*current grid answer legal test*)
    answer_current_board_test "board1 answer 1 in row 2 col 2" 2 2 1 board1_init
      board1_grid_2_2_1
    (*current grid answer illegal test*);
    answer_current_board_test "board1 answer 1 in row 12 col 2" 12 2 1
      board1_init [];
    answer_current_board_test "board1 answer 1 in row 2 col 12" 2 12 1
      board1_init [];
    answer_current_board_test "board1 answer 10 in row 2 col 2" 2 2 10
      board1_init [];
    answer_current_board_test "board1 answer 1 in row 1 col 1" 1 1 1 board1_init
      [];
  ]

let suite =
  "test suite for A2"
  >::: List.flatten [ grid_tests; command_tests; state_tests ]

let _ = run_test_tt_main suite
