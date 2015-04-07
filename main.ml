module Gyges = Game.Make(struct
  type board = Board.t
  type player = Player.t
  type move = Cell.position * Cell.position

  let empty_board = Board.empty

  let default_players = [ Player.TopSide ; Player.BotSide ]

  let prepare board player =
    let pieces = Player.pieces_to_add ~board ~player in
    let place_piece board piece =
      let rec read_position () =
        Printf.printf "Select cell of %s, type for instance: '0,1'\n" (Board.string_of_piece ~piece) ;
        let str = read_line () in
        match Core.Std.String.split ~on:',' str with
          | i::j::[] -> Cell.Matrix (int_of_string i, int_of_string j)
          | _ -> read_position ()
      in
      Printf.printf "\x1B[2J\x1B[0;0fYou are the %s player\n" (Player.string_of_player ~player) ;
      Board.print ~board ~highlight:[] ;
      let position = read_position () in
      Board.add ~board ~piece ~position
    in
    List.fold_left place_piece board pieces

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

  let read_move board player _ =
    Printf.printf "\x2B[2J\x1B[0;0fYou are the %s player\n" (Player.string_of_player ~player) ;
    Board.print ~board ~highlight:[] ;
    let rec read_start () =
      Printf.printf "Select start cell, type for instance: '0,1'\n" ;
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
