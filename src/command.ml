type coordinates = {
  row : int;
  col : int;
}

type move_phrase = {
  row : int;
  col : int;
  value : int;
}

type command =
  | Move of move_phrase
  | Delete of coordinates
  | Solve
  | Hint
  | Quit

exception Empty
exception Malformed

let list_to_string lst = List.map (fun x -> int_of_string x) lst

let parse (str : string) : command =
  let filtered =
    List.filter (fun str -> str <> "") (String.split_on_char ' ' str)
  in
  match filtered with
  | [] -> raise Empty
  | "place" :: t -> (
      let vals = list_to_string t in
      match List.length vals with
      | 3 ->
          Move
            {
              row = List.nth vals 0;
              col = List.nth vals 1;
              value = List.nth vals 2;
            }
      | _ -> raise Malformed)
  | "delete" :: t -> (
      let vals = list_to_string t in
      match List.length vals with
      | 2 -> Delete { row = List.nth vals 0; col = List.nth vals 1 }
      | _ -> raise Malformed)
  | [ "solve" ] -> Solve
  | [ "hint" ] -> Hint
  | [ "quit" ] -> Quit
  | _ -> raise Malformed
