let _ = GtkMain.Main.init ()

class cell ~dimension:(width,height) ~text ~packing ~position =
  let _box = GBin.event_box ~packing ~width ~height () in
  let _label = GMisc.label ~text ~packing:_box#add () in

  object (self)
    val box = _box
    val label = _label
    val position : Cell.position = position

    val mutable on_click = None

    initializer box#event#add [`BUTTON_RELEASE]

    method position = position

    method del_on_click =
      match on_click with
        | None -> ()
        | Some signal -> GtkSignal.disconnect box#as_widget signal

    method set_on_click ~callback =
      self#del_on_click ;
      on_click <- Some (box#event#connect#button_release ~callback)

    method set_text new_text =
      label#set_text new_text

  end

class gyges_ui =

  (* Build the UI. *)
  let _window = GWindow.window ~title:"Gyges" () in
  let quit _ = print_endline "Exiting Gyges..." ; flush stdout ; false in
  let _ = _window#event#connect#delete ~callback:quit in
  let _ = _window#connect#destroy ~callback:GMain.Main.quit in
  let layout = GPack.vbox ~packing:_window#add ~border_width:5 () in
  let top_cell = new cell ~dimension:(300,50) ~text:"_" ~packing:layout#add ~position:Cell.Top in
  let grid = GPack.table ~packing:layout#add ~width:300 ~height:300 ~rows:6 ~columns:6 () in
  let bot_cell = new cell ~dimension:(300,50) ~text:"_" ~packing:layout#add ~position:Cell.Bot in
  let _status = GMisc.label ~text:"Welcome to Gyges!" ~packing:layout#add () in
  let _cells = ref [top_cell ; bot_cell] in
  let _ =
    for i = 0 to 5 do
      for j = 0 to 5 do
        let cell = new cell
          ~dimension:(50,50)
          ~text:"_"
          ~packing:(grid#attach ~left:j ~top:i)
          ~position:(Cell.Matrix (i, j))
        in
        _cells := cell::!_cells
      done
    done
  in

  object (self)
    val window = _window
    val cells = !_cells
    val status = _status

    method show () = window#show ()

    method announce message =
      status#set_text message

    method sync_board ~board =
      ListLabels.iter cells ~f:(fun cell ->
        let piece = Board.piece_at ~board ~position:cell#position in
        cell#set_text (match piece with None -> "_" | Some p -> Board.string_of_piece p))

    method wait_for_position () =
      let input = ref None in
      let rec wait () =
        match !input with
          | None -> ignore (Unix.select [] [] [] 0.04) ; wait ()
          | Some position -> position
      in
      let setup_on_click cell =
        cell#set_on_click ~callback:(fun _ -> input := Some cell#position ; true)
      in
      ListLabels.iter cells ~f:setup_on_click ;
      wait ()

  end

let ui = new gyges_ui

module GygesUI = Game.Make(struct
  type board = Board.t
  type player = Player.t
  type move = Cell.position * Cell.position

  let empty_board = Board.empty
  let default_players = [ Player.TopSide ; Player.BotSide ]
  let human_players = [ Player.TopSide ]

  let prepare board player =
    let pieces = Player.pieces_to_add ~board ~player in
    let place_piece board piece =
      let rec read_column () =
        let message = Printf.sprintf
          "Select column for piece %s for player %s"
          (Board.string_of_piece ~piece)
          (Player.string_of_player ~player)
        in
        let _ = ui#announce message in
        let position = ui#wait_for_position () in
        let position = match player, position with
          | Player.TopSide, Cell.Matrix (_, col) -> Cell.Matrix (0, col)
          | Player.BotSide, Cell.Matrix (_, col) -> Cell.Matrix (5, col)
          | _, _ -> read_column ()
        in
        if Board.is_empty ~board ~position then position
        else read_column ()
      in
      let position = read_column () in
      let new_board = Board.add ~board ~piece ~position in
      ui#sync_board ~board:new_board ;
      new_board
    in
    List.fold_left place_piece board pieces

  let moves board player =
    let starts = Player.possible_positions ~board ~player in
    ListLabels.fold_left starts ~init:[] ~f:(fun acc start ->
      let cells = Move.possible_stops ~board ~player ~start in
      let pairs = List.map (fun (cell:Cell.t) -> (start, cell.position)) cells in
      pairs @ acc)

  let is_winning player (_, stop) =
    match player, stop with
      | (Player.TopSide, Cell.Bot) -> true
      | (Player.BotSide, Cell.Top) -> true
      | _ -> false

  let play board (start, stop) =
    match Board.piece_at ~board ~position:start with
      | None -> raise (Board.NoPieceFoundAt start)
      | Some piece -> Board.move ~board ~piece ~start ~stop

  let read_move board player possible_moves =
    ui#sync_board ~board ;
    let rec read_start () =
      ui#announce "Select start cell" ;
      let position = ui#wait_for_position () in
      match position with
        | Cell.Matrix _ -> position
        | _ -> read_start ()
    and read_stop () =
      ui#announce "Select stop cell" ;
      ui#wait_for_position ()
    in
    let rec select_move () =
      let start = read_start () in
      let stop = read_stop () in
      let move = (start, stop) in
      if List.mem move possible_moves then move
      else select_move ()
    in
    select_move ()

  let announce_move player (start, stop) =
    let message = Printf.sprintf
      "Player %s just played: %s -> %s"
      (Player.string_of_player ~player)
      (Cell.string_of_position start)
      (Cell.string_of_position stop)
    in
    ui#announce message

  let announce_endgame board player =
    let message = Printf.sprintf
      "Player %s just won!"
      (Player.string_of_player ~player)
    in
    ui#sync_board ~board ;
    ui#announce message

end)

let start_game () =
  let initial_state = GygesUI.start () in
  GygesUI.game_loop initial_state

let () =
  ui#show () ;
  let game_loop = Thread.create start_game () in
  GMain.Main.main () ;
