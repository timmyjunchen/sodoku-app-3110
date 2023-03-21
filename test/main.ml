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

let current_board_test name adv expected_output : test =
  name >:: fun _ -> assert_equal expected_output (current_board adv)

let grid_tests =
  [
    (*start board test*)
    start_board_test "board1 start board" board1 board1_grid;
    (*current board test*)
    current_board_test "board1 current board" board1 board1_grid;
    ({|print board test|} >:: fun _ -> assert_equal () (print_board board1));
    ( {|row test|} >:: fun _ ->
      assert_equal [ 2; 0; 0; 3; 0; 0; 0; 0; 0 ] (get_row board1 1) );
    ( {|col test|} >:: fun _ ->
      assert_equal [ 0; 0; 1; 0; 0; 3; 2; 0; 0 ] (get_col board1 2) );
    ( {|block test 1|} >:: fun _ ->
      assert_equal [ 2; 0; 0; 8; 0; 4; 0; 1; 3 ] (get_block board1 (1, 1)) );
    ( {|block test 2|} >:: fun _ ->
      assert_equal [ 0; 0; 9; 2; 5; 0; 0; 0; 1 ] (get_block board1 (3, 2)) );
    ({|cell test 1|} >:: fun _ -> assert_equal 0 (get_cell board1 (6, 9)));
    ({|cell test 2|} >:: fun _ -> assert_equal 2 (get_cell board1 (4, 5)));
  ]

let command_tests =
  [
    ( {|parse command place 5 2 3|} >:: fun _ ->
      assert_equal (Move { value = 5; row = 2; col = 3 }) (parse "place 5 2 3")
    );
  ]

(*state tests*)
let current_grid_test name state expected_output : test =
  name >:: fun _ -> assert_equal expected_output (current_grid state)

let state_tests =
  let board1_init = init_state board1 in
  [ current_grid_test "board 1 initial state" board1_init board1_grid ]

let suite =
  "test suite for A2"
  >::: List.flatten [ grid_tests; command_tests; state_tests ]

let _ = run_test_tt_main suite
