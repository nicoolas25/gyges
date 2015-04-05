type exploration_step =
  {
    current : Board.cell ;
    previous : Board.cell option ;
    remaining : int ;
  }

let range ~piece =
  match piece with
    | Board.PieceOne -> 1
    | Board.PieceTwo -> 2
    | Board.PieceThree -> 3

let stops ~from ~distance =
  let find_candidate_cells step =
    let is_previous cell =
      match step.previous with
        | None -> false
        | Some prev_cell -> Board.compare prev_cell cell
    in
    let candidate_selector cell =
      if is_previous cell then false
      else match cell.position with
        | Top | Bot -> step.remaining = 1
        | Matrix _ -> true
    in
    List.filter candidate_selector step.current.edges
  in
  let rec walk stops explorable =
    match explorable with
      | [] -> stops
      | step::rest ->
          begin
            match step.remaining with
              | 0 -> walk (step.current::stops) rest
              | n ->
                  let build_step cell =
                    {
                      current = cell ;
                      previous = Some step.current ;
                      remaining = n - 1 ;
                    }
                  in
                  let candidate_cells = find_candidate_cells step in
                  let neighbors = List.map build_step candidate_cells in
                  walk stops (neighbors @ rest)
          end
  in
  let initial_step =
    {
      current = from ;
      previous = None ;
      remaining = distance ;
    }
  in
  walk [] [initial_step]

let possible_stops ~board ~start =
  match Board.piece_at ~board ~position:start with
    | None -> []
    | Some(piece) ->
        let cell = Board.cell_at ~board ~position:start in
        let distance = range ~piece in
        stops ~from:cell ~distance
