open Grid

type t = { current_grid : int list list }

let init_state board = { current_grid = print_board board }
let current_grid st = st.current_grid

type result =
  | Legal of t
  | Illegal

let answer row col number st =
  try
    let current_grid = current_grid st in
    let new_grid = next_grid current_grid row col number in
    let st' = { current_grid = new_grid } in
    Legal st'
  with InvalidBox _ | InvalidAnswer _ -> Illegal
