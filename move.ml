let range ~piece =
  match piece with
    | Board.PieceOne -> 1
    | Board.PieceTwo -> 2
    | Board.PieceThree -> 3

let stops ~from ~distance =
  let rec walk stops (explorable:(Board.cell * int) list) =
    match explorable with
      | [] -> stops
      | (cell, distance)::rest ->
          begin
            match distance with
              | 0 -> walk (cell::stops) rest
              | n ->
                  let add_distance cell = (cell, n - 1) in
                  let neighbors = List.map add_distance cell.edges in
                  walk stops (neighbors @ rest)
          end
  in
  walk [] [(from, distance)]

let possible_stops ~board ~start =
  match Board.piece_at ~board ~position:start with
    | None -> []
    | Some(piece) ->
        let cell = Board.cell_at ~board ~position:start in
        let distance = range ~piece in
        stops ~from:cell ~distance
