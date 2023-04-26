let rec print_list lst =
  match lst with
  | [] -> ()
  | h :: t ->
      let _ = print_int h in
      print_list t

let shuffle lst =
  let assoc = List.map (fun e -> (Random.int 10000000, e)) lst in
  let shuffled =
    List.sort (fun e1 e2 -> if fst e1 < fst e2 then -1 else 1) assoc
  in
  let result = List.map (fun e -> snd e) shuffled in
  let _ = print_list result in
  result
