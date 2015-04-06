type t = TopSide | BotSide

let possible_positions ~(board:Board.t) ~player =
  let rec find_cells step = function
    | -1 | 6 -> []
    | index ->
      let line = board.matrix.(index) in
      let cells =
        ArrayLabels.fold_left line ~init:[] ~f:(fun l (cell:Cell.t) ->
          if Board.is_empty ~board ~position:cell.position then l
          else cell.position::l)
      in
      if cells = [] then find_cells step (index + step) else cells
  in
  match player with
    | TopSide -> find_cells 1 0
    | BotSide -> find_cells (-1) 5
