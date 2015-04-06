type exploration_step =
  {
    current : Board.cell ;
    history : exploration_step list ;
    remaining : int ;
  }

let range ~piece =
  match piece with
    | Board.PieceOne -> 1
    | Board.PieceTwo -> 2
    | Board.PieceThree -> 3

let stops ~board ~from ~distance =
  let find_candidate_cells step =
    let is_previous cell =
      match step.history with
        | [] -> false
        | last_step::_ -> Board.compare cell last_step.current
    in
    let is_loop _ =
      false
    in
    let candidate_selector cell =
      if is_previous cell then false
      else if is_loop cell then false
      else
        match cell.position, step.remaining with
          | Top, 1 | Bot, 1 -> true
          | Top, _ | Bot, _ -> false
          | _, 1 -> true
          | _, _ -> Board.is_empty ~board ~position:cell.position
    in
    List.filter candidate_selector step.current.edges
  in
  let rec walk stops explorable =
    match explorable with
      | [] -> stops
      | step::rest ->
          begin
            match step.remaining with
              | 0 ->
                  begin
                    match Board.piece_at ~board ~position:step.current.position with
                      | None -> walk (step.current::stops) rest
                      | Some piece ->
                          let jump = { step with remaining = (range ~piece) } in
                          walk stops (jump::rest)
                  end
              | n ->
                  let build_step cell =
                    {
                      current = cell ;
                      history = step::step.history ;
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
      history = [] ;
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
        stops ~board ~from:cell ~distance
