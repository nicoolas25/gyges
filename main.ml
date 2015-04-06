module Gyges = Game.Make(struct
  type board = Board.t
  type player = Player.t
  type move = Cell.position * Cell.position

  let empty_board = Board.empty

  let default_players = [ Player.TopSide ; Player.BotSide ]

  let prepare board _ = board

  let moves board player =
    let starts = Player.possible_positions ~board ~player in
    ListLabels.fold_left starts ~init:[] ~f:(fun acc start ->
      let cells = Move.possible_stops ~board ~start in
      let pairs = List.map (fun (cell:Cell.t) -> (start, cell.position)) cells in
      pairs @ acc)

  let play board (start, stop) =
    match Board.piece_at ~board ~position:start with
      | None -> raise (Board.NoPieceFoundAt start)
      | Some piece -> Board.move ~board ~piece ~start ~stop

  let read_move board _ _ =
    Board.print ~board ~highlight:[] ;
    let rec read_start () =
      print_endline "Select start cell, type for instance: '0,1'" ;
      let str = read_line () in
      match Core.Std.String.split ~on:',' str with
        | i::j::[] -> Cell.Matrix (int_of_string i, int_of_string j)
        | _ -> read_start()
    and read_stop () =
      print_endline "Select stop cell, type for instance: '0,1', 'T' or 'B'" ;
      let str = read_line () in
      match Core.Std.String.split ~on:',' str with
        | "T"::[] -> Cell.Top
        | "B"::[] -> Cell.Bot
        | i::j::[] -> Cell.Matrix (int_of_string i, int_of_string j)
        | _ -> read_stop()
    in
    let start = read_start () in
    let stop = read_stop () in
    (start, stop)

end)

let () =
  let initial_state = Gyges.start () in
  Gyges.game_loop initial_state
