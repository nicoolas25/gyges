let () =
  let board = Board.empty
  and piece = Board.PieceOne
  and position = (0, 0) in
  let board = Board.add ~board ~piece ~position in
  Board.print ~board
