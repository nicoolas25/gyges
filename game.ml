module type State = sig
  type board
  type player
  type move

  val empty_board : board

  val default_players : player list

  (* This function will be called for each player before the game begins.
   * The resulting board will be used after that. *)
  val prepare : board -> player -> board

  (* This function should return a list of valid moves that the given player
   * can do on the given board. *)
  val moves : board -> player -> move list

  (* This function should apply a move on a board and return the resulting
   * board. It should be purely functionnal. *)
  val play : board -> move -> board

  (* This function is called for human players only in order to select a move.
   * In order to help, the list of valid move is given too. *)
  val read_move : board -> player -> move list -> move
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
    let board = List.fold_right S.prepare board players in
    {
      board = board ;
      players = Array.of_list players ;
      current_player_index = 0 ;
    }

  let change_player state =
    let naive = state.current + 1 in
    let index = if naive >= (Array.length state.players) then 0 else naive in
    { state with current = index }

  let rec game_loop state =
    let current_player = state.players.(state.current_player_index) in
    let moves = S.moves state.board current_player in
    let move = S.read_move state.board current_player moves in
    let next_board = S.play state.board move in
    let next_state = change_player state in
    game_loop { next_state with board = next_board }

end
