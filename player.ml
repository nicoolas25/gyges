type t = TopSide | BotSide

let string_of_player ~player =
  match player with
    | TopSide -> "Top"
    | BotSide -> "Bottom"

let pieces_to_add ~(board:Board.t) ~player =
  let expected_pieces = [
    Board.PieceOne ; Board.PieceOne ; Board.PieceTwo ;
    Board.PieceTwo ; Board.PieceThree ; Board.PieceThree ] in
  let line = match player with TopSide -> 0 | BotSide -> 5 in
  let cells = Array.to_list board.matrix.(line) in
  let found_pieces = ListLabels.fold_left cells ~init:[] ~f:(fun acc (cell:Cell.t) ->
    match Board.piece_at ~board ~position:cell.position with
      | None -> acc
      | Some piece -> piece :: acc) in
  let rec remove_one_element acc l e =
    match l with
      | [] -> acc
      | h::t when h = e -> acc @ t
      | h::t -> remove_one_element (h::acc) t e
  in
  List.fold_left (remove_one_element []) expected_pieces found_pieces

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
