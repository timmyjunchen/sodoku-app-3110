open OUnit2
open Sudoku
open Grid

let board1 : int list list =
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

let grid_tests =
  [
    ({|print board test|} >:: fun _ -> assert_equal () (print_board board1));
    ( {|row test|} >:: fun _ ->
      assert_equal [ 2; 0; 0; 3; 0; 0; 0; 0; 0 ] (get_row board1 1) );
    ( {|col test|} >:: fun _ ->
      assert_equal [ 0; 0; 1; 0; 0; 3; 2; 0; 0 ] (get_col board1 2) );
    ( {|block test 1|} >:: fun _ ->
      assert_equal [ 2; 0; 0; 8; 0; 4; 0; 1; 3 ] (get_block board1 (1, 1)) );
    ( {|block test 2|} >:: fun _ ->
      assert_equal [ 0; 0; 9; 2; 5; 0; 0; 0; 1 ] (get_block board1 (3, 2)) );
  ]

let suite = "test suite for A2" >::: List.flatten [ grid_tests ]
let _ = run_test_tt_main suite
