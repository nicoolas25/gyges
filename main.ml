let () =
  let board = Board.empty
  and piece = Board.PieceTwo
  and position = Cell.Matrix (0, 0)
  and target = Cell.Matrix (5, 2) in
  let board = Board.add ~board ~piece ~position in
  let board = Board.add ~board ~piece ~position:(Cell.Matrix (3, 2)) in
  let board = Board.add ~board ~piece ~position:(Cell.Matrix (3, 4)) in
  let board = Board.add ~board ~piece ~position:(Cell.Matrix (5, 4)) in
  let board = Board.move ~board ~piece ~start:position ~stop:target in
  Board.print ~board ~highlight:[] ;

  (* let possible_stops = Move.possible_stops ~board ~start:target in
  Printf.printf "%d possibles moves:\n" (List.length possible_stops) ;
  Board.print ~board ~highlight:possible_stops *)

  Printf.printf "possible positions for top:\n" ;
  let positions = Player.possible_positions ~board ~player:Player.TopSide in
  List.iter (fun p -> print_endline (Cell.string_of_position p)) positions ;

  Printf.printf "possible positions for bot:\n" ;
  let positions = Player.possible_positions ~board ~player:Player.BotSide in
  List.iter (fun p -> print_endline (Cell.string_of_position p)) positions

