module CellSet = Set.Make(Cell)

type exploration_step =
  {
    current : Cell.t ;
    history : exploration_step list ;
    remaining : int ;
  }

let compare (step_1:exploration_step) (step_2:exploration_step) =
  let cmp_1 = Cell.compare step_1.current step_2.current in
  if cmp_1 = 0
  then
    let cmp_2 = Pervasives.compare step_1.remaining step_2.remaining in
    if cmp_2 = 0
    then
      match step_1.history, step_2.history with
        | [], [] -> 0
        | [], _ -> -1
        | _, [] -> 1
        | p1::_, p2::_ -> Cell.compare p1.current p2.current
    else cmp_2
  else cmp_1

let range ~piece =
  match piece with
    | Board.PieceOne -> 1
    | Board.PieceTwo -> 2
    | Board.PieceThree -> 3

let find_next_steps ~board ~player ~step =
  let is_uturn next_step =
    match step.history with
      | [] -> false
      | last_step::_ -> Cell.equals next_step.current last_step.current

  and is_already_done next_step =
    List.exists
      (fun past_step -> (compare past_step next_step) = 0)
      step.history

  and is_fobidden next_step =
    match next_step.current.position, next_step.remaining, player with
      | Top, 0, Player.BotSide -> false
      | Bot, 0, Player.TopSide -> false
      | Top, _, _ -> true
      | Bot, _, _ -> true
      | _, 0, _ -> false
      | p, _, _ -> not (Board.is_empty ~board ~position:p)

  and all_next_steps =
    List.map
      (fun cell -> {
        current = cell ;
        history = (step::step.history) ;
        remaining = step.remaining - 1 ;
      })
      step.current.edges
  in

  List.filter
    (fun next_step ->
      not ((is_uturn next_step)
        || (is_already_done next_step)
        || (is_fobidden next_step)))
    all_next_steps

let stops ~board ~player ~from ~distance =
  let rec walk stops explorable =
    match explorable with
      | [] -> stops
      | step::rest when step.remaining = 0 ->
          begin
            match Board.piece_at ~board ~position:step.current.position with
              | None -> walk (CellSet.add step.current stops) rest
              | Some piece ->
                  let jump = { step with remaining = (range ~piece) } in
                  walk stops (jump::rest)
          end
      | step::rest ->
          let next_steps = find_next_steps ~board ~player ~step in
          walk stops (next_steps @ rest)
  in
  let initial_step = { current = from ; history = [] ; remaining = distance } in
  walk CellSet.empty [initial_step]

let possible_stops ~board ~player ~start =
  match Board.piece_at ~board ~position:start with
    | None -> []
    | Some(piece) ->
        let cell = Board.cell_at ~board ~position:start in
        let distance = range ~piece in
        let cell_set = stops ~board ~player ~from:cell ~distance in
        CellSet.fold (fun cell l -> cell::l) cell_set []
