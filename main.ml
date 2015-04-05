let () =
  let board = Board.empty
  and piece = Board.PieceOne
  and position = (0, 0) in
  let board = Board.add ~board ~piece ~position in
  let board = Board.move ~board ~piece ~start:position ~stop:(1, 0) in
  Board.print ~board
