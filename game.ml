module type State = sig
  type board
  type player
  type move

  val empty_board : board

  val default_players : player list

  val human_players : player list

  (* This function will be called for each player before the game begins.
   * The resulting board will be used after that. Even non human player
   * are passed to this function. *)
  val prepare : board -> player -> board

  (* This function should return a list of valid moves that the given player
   * can do on the given board. *)
  val moves : board -> player -> move list

  (* Tells if a move is a winning one. *)
  val is_winning : player -> move -> bool

  (* This function should apply a move on a board and return the resulting
   * board. It should be purely functionnal. *)
  val play : board -> move -> board

  (* This function is called for human players only in order to select a move.
   * In order to help, the list of valid move is given too. *)
  val read_move : board -> player -> move list -> move

  (* This function is called after a move is played. *)
  val announce_move : player -> move -> unit

  (* This function is called after the game is won. *)
  val announce_endgame : player -> unit
end

module Make (S:State) = struct
  type state =
    {
      board : S.board ;
      players : S.player array ;
      current_player_index : int ;
    }

  let start () =
    let board = S.empty_board
    and players = S.default_players in
    let board = List.fold_left S.prepare board players in
    {
      board = board ;
      players = Array.of_list players ;
      current_player_index = 0 ;
    }

  let change_player state =
    let naive = state.current_player_index + 1 in
    let index = if naive >= (Array.length state.players) then 0 else naive in
    { state with current_player_index = index }

  let rec best_move =
    let next_moves state =
      let player = state.players.(state.current_player_index) in
      List.map (fun move -> (player, move)) (S.moves state.board player)
    in
    let rec find_winning_move moves =
      match moves with
        | [] -> None
        | (player, move)::rest ->
            if S.is_winning player move then Some move
            else find_winning_move rest
    in
    let apply state (_, move) =
      { (change_player state) with board = (S.play state.board move) }
    in
    let random_move moves =
      let _, move = List.nth moves (Random.int (List.length moves)) in
      move
    in
    (fun state depth ->
      let possible_moves = next_moves state in
      match find_winning_move possible_moves with
        | Some move -> move
        | None when depth = 0 -> random_move possible_moves
        | None ->
            let candidates = ListLabels.filter possible_moves ~f:(fun move ->
              let next_state = apply state move in
              let opponent = next_state.players.(next_state.current_player_index) in
              let opponent_move = best_move next_state (depth - 1) in
              not (S.is_winning opponent opponent_move))
            in
            random_move (match candidates with [] -> possible_moves | _ -> candidates)
    )

  let rec game_loop state =
    let current_player = state.players.(state.current_player_index) in
    let move =
      if List.mem current_player S.human_players then
        let possible_moves = S.moves state.board current_player in
        S.read_move state.board current_player possible_moves
      else
        best_move state 0
    in
    let next_board = S.play state.board move in
    let () = S.announce_move current_player move in
    if S.is_winning current_player move then
      S.announce_endgame current_player
    else
      let next_state = change_player state in
      game_loop { next_state with board = next_board }

end
