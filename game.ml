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

  let random_elt elements =
    List.nth elements (Random.int (List.length elements))

  let select_best_move state player =
    let possible_moves = S.moves state.board player in
    match List.find_all (S.is_winning player) possible_moves with
      | move::_ -> move
      | [] -> random_elt possible_moves

  let rec game_loop state =
    let current_player = state.players.(state.current_player_index) in
    let move =
      if List.mem current_player S.human_players then
        let possible_moves = S.moves state.board current_player in
        S.read_move state.board current_player possible_moves
      else
        select_best_move state current_player
    in
    let next_board = S.play state.board move in
    let () = S.announce_move current_player move in
    let next_state = change_player state in
    game_loop { next_state with board = next_board }

end
