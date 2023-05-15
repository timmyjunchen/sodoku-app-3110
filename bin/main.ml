open Sudoku
open State
open Boardmaker
(* open Command *)

let board1_grid = generate_board 16 5
let board1 = init_state board1_grid

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  let play board (cmd : Command.command) =
    match cmd with
    | Move phrase -> (
        let rslt = answer phrase.row phrase.col phrase.value board in
        match rslt with
        | Legal brd -> ("Valid move. Nice!", brd)
        | Illegal -> ("\n\nMove is Illegal, please try again. \n", board))
    | Delete coords -> (
        let rslt = delete coords.row coords.col board in
        match rslt with
        | Legal brd -> ("Value deleted!", brd)
        | Illegal ->
            ("\n\nCannot delete values in that cell. Try Again \n", board))
    | Solve -> ("\n\n Here is your solved board", solve_board board)
    | Hint -> (
        match board_hint board with
        | Legal brd -> ("Here's a hint!", brd)
        | Illegal -> ("The board is unsolvable", board))
    | Quit -> Stdlib.exit 0
  in
  let rec prompt str board =
    ANSITerminal.print_string [] "\n\nHere is your Sodoku Board.\n";
    print_board board;
    if check_win board then
      ANSITerminal.print_string [ ANSITerminal.red ] "You win!!"
    else ANSITerminal.print_string [ ANSITerminal.red ] str;
    print_endline
      "\n\
       Please enter what move you want to make! To answer, type place [row] \
       [col] [answer]\n";
    print_endline "\nYou can delete entries by typing delete [row] [col] \n";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | str -> (
        try
          let x = Command.parse str |> play board in
          match x with
          | str, brd -> prompt str brd
        with Command.Malformed ->
          prompt
            "\n\n\
             That is not a valid command \n\
             Make sure to style commands in the form of 'place row col val', \
             'delete row col', or 'quit'"
            board)
  in
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 Text\n Sudoku Game engine.\n";
  prompt "" board1

(* Execute the game engine. *)
let () = main ()
