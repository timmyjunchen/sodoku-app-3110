open OUnit2
open Sudoku
open Grid

let board1 : int list list = [ [ 2; 10; 10 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ]

let grid_tests =
  [
    ({|init_state ho plaza|} >:: fun _ -> assert_equal () (print_block board1));
  ]

let suite = "test suite for A2" >::: List.flatten [ grid_tests ]
let _ = run_test_tt_main suite
