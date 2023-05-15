open Sudoku
open State
open Boardmaker
(* open Command *)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  let time = ref 0.0 in
  let rec play board (cmd : Command.command) =
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
    | Help ->
        ( "\n\
           \"place [row] [col] [num]\": Places a number at row and col.\n\
           \"delete [row] [col]\": Deletes a number at row and col.\n\
           \"solve\": Solves the board, not guaranteed to finish for 16x16.\n\
           \"hint\": Fills in a single number, randomly in the board.\n\
           \"quit\": Quits the game of Sodoku.",
          board )
    | Quit -> Stdlib.exit 0
    | _ -> raise Command.Malformed
  and prompt str board =
    ANSITerminal.print_string [] "\n\nHere is your Sodoku Board.\n";
    print_board board;
    if check_win board then (
      ANSITerminal.print_string [ ANSITerminal.red ] "You win!!\n";
      ANSITerminal.print_string [ ANSITerminal.blue ]
        ("It took you "
        ^ string_of_float (Unix.time () -. !time)
        ^ " seconds to solve the puzzle. Good job!\n");
      ANSITerminal.print_string []
        "What would you like to do now?\n\
         Type \"play\" to play again or \"quit\" to quit\n\n\
         >";
      match read_line () with
      | exception End_of_file -> ()
      | "play" -> board_size_prompt ()
      | "quit" -> exit 0
      | _ ->
          ANSITerminal.print_string []
            "Invalid input, I assume you quit. Good job though!\n";
          exit 0)
    else (
      ANSITerminal.print_string [ ANSITerminal.red ] str;
      print_endline
        "\n\
         Please enter what move you want to make! To answer, type place [row] \
         [col] [answer]\n";
      print_endline "Type \"help\" for a list of all commands! \n";
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
              board))
  and board_difficulty_prompt size =
    ANSITerminal.print_string []
      "What difficulty would you like? Please enter \"easy\", \"medium\", \
       \"hard\" or \"back\" to go back.\n\n\
       > ";
    match read_line () with
    | exception End_of_file -> ()
    | "easy" ->
        time := Unix.time ();
        prompt "" (init_state (generate_board size 1))
    | "medium" ->
        time := Unix.time ();
        prompt "" (init_state (generate_board size 3))
    | "hard" ->
        time := Unix.time ();
        prompt "" (init_state (generate_board size 10))
    | "back" -> board_size_prompt ()
    | "quit" -> Stdlib.exit 0
    | _ ->
        ANSITerminal.print_string [] "That is not a valid command\n\n";
        board_difficulty_prompt size
  and board_size_prompt () =
    ANSITerminal.print_string []
      "What size board would you like? Please enter \"small\", \"medium\", or \
       \"large\".\n\n\
       > ";
    match read_line () with
    | exception End_of_file -> ()
    | "small" -> board_difficulty_prompt 4
    | "medium" -> board_difficulty_prompt 9
    | "large" -> board_difficulty_prompt 16
    | "quit" -> Stdlib.exit 0
    | _ ->
        ANSITerminal.print_string [] "That is an invalid input \n\n";
        board_size_prompt ()
  in
  let board_enter_mode size =
    let board = init_state (Array.make_matrix size size 0) in
    time := Unix.time ();
    prompt "" board
  in
  let solver () =
    ANSITerminal.print_string []
      "What size board do you want to solve? Please enter \"small\"for a 4x4, \
       \"medium\" for a 9x9, or \"large\" for a 16x16.\n\n\
       > ";
    match read_line () with
    | exception End_of_file -> ()
    | "small" -> board_enter_mode 4
    | "medium" -> board_enter_mode 9
    | "large" -> board_enter_mode 16
    | "quit" -> Stdlib.exit 0
    | _ -> ANSITerminal.print_string [] "That is an invalid input \n\n"
  in
  let file_parser () =
    ANSITerminal.print_string []
      "Please enter the file path to the board you want to use. Please make \
       sure is in the format of a json file! \n\n\
       > ";
    match read_line () with
    | exception End_of_file -> ()
    | path ->
        time := Unix.time ();
        prompt "" (init_state (Yojson.Basic.from_file path |> from_json))
  in
  let rec parse_begin str =
    try
      match Command.parse str with
      | PlayMode -> board_size_prompt ()
      | SolveMode -> solver ()
      | FileMode -> file_parser ()
      | _ -> raise Command.Malformed
    with Command.Malformed -> (
      ANSITerminal.print_string []
        "That is not a valid command, please enter \"play\" to play a game of \
         Sodoku, \"solver\" to enter and solve \n\
         a Sodoku board, or \"file\" to solve a Sodoku board in a json file.\n\n\
         > ";
      match read_line () with
      | exception End_of_file -> ()
      | str -> parse_begin str)
  in
  let start () =
    ANSITerminal.print_string [ ANSITerminal.red ]
      "\n\nWelcome to the Sudoku Game engine.\n\nWhat would you like to do?\n";
    ANSITerminal.print_string []
      "Please enter \"play\" to play a game of Sodoku, \"solver\" to enter and \
       solve a Sodoku board, or \"file\" to solve a Sodoku board in a json \
       file.\n\n\
       > ";
    match read_line () with
    | exception End_of_file -> ()
    | str -> parse_begin str
  in
  start ()

(* Execute the game engine. *)
let () = main ()
