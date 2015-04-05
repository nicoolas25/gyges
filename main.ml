let () =
  let board = Board.empty
  and piece = Board.PieceOne
  and position = Board.Matrix (0, 0)
  and target = Board.Matrix (5, 2) in
  let board = Board.add ~board ~piece ~position in
  let board = Board.move ~board ~piece ~start:position ~stop:target in
  let possible_stops = Move.possible_stops ~board ~start:target in
  Board.print ~board ~highlight:[] ;
  Printf.printf "%d possibles moves:\n" (List.length possible_stops) ;
  Board.print ~board ~highlight:possible_stops
