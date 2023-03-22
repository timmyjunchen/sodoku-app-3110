open Sudoku
open Grid
open State
(* open Command *)

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

let board1 = init_state (board_setup board1_grid)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  let play board (cmd : Command.command) =
    match cmd with
    | Move phrase -> (
        let rslt = answer phrase.row phrase.col phrase.value board in
        match rslt with
        | Legal brd -> brd
        | Illegal ->
            ANSITerminal.print_string [ ANSITerminal.red ]
              "\n\nMove is Illegal, please try again. \n";
            board)
    | Quit -> Stdlib.exit 0
  in
  let rec prompt str board =
    ANSITerminal.print_string [] "\n\nHere is your Sodoku Board.\n";
    print_board board;
    ANSITerminal.print_string [ ANSITerminal.red ] str;
    print_endline
      "\n\
       Please enter what move you want to make! To answer, type place row col \
       answer\n";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | str -> (
        try Command.parse str |> play board |> prompt "Valid move. Nice!"
        with Command.Malformed ->
          prompt
            "\n\n\
             That is not a valid command \n\
             Make sure to style commands in the form of 'place row col val' or \
             'quit'"
            board)
  in
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 Text\n Sudoku Game engine.\n";
  prompt "" board1

(* Execute the game engine. *)
let () = main ()
