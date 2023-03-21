type move_phrase = {
  row : int;
  col : int;
  value : int;
}

type command =
  | Move of move_phrase
  | Quit

exception Empty
exception Malformed

let parse (str : string) : command =
  let filtered =
    List.filter (fun str -> str <> "") (String.split_on_char ' ' str)
  in
  match filtered with
  | [] -> raise Empty
  | "place" :: t ->
      let vals = List.map (fun x -> int_of_string x) t in
      Move
        {
          row = List.nth vals 0;
          col = List.nth vals 1;
          value = List.nth vals 2;
        }
  | [ "quit" ] -> Quit
  | _ -> raise Malformed
