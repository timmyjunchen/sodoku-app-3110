open Yojson.Basic.Util

let from_json json =
  let json_list =
    json |> member "board" |> to_list
    |> List.map (fun row -> to_list row |> List.map to_int)
  in
  let size = List.length json_list in
  let new_board = Array.init size (fun _ -> Array.init size (fun _ -> 0)) in
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      new_board.(i).(j) <- List.nth (List.nth json_list i) j
    done
  done;
  new_board
