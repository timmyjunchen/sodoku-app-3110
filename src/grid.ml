open Yojson.Basic.Util

let from_json json =
  let json_list =
    json |> member "board" |> to_list
    |> List.map (fun row -> to_list row |> List.map to_int)
  in
  let new_board = Array.init 9 (fun _ -> Array.init 9 (fun _ -> 0)) in
  for i = 0 to 8 do
    for j = 0 to 8 do
      new_board.(i).(j) <- List.nth (List.nth json_list i) j
    done
  done;
  new_board
