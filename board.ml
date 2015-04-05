type position = int * int

type piece = PieceOne | PieceTwo | PieceThree

type cell =
  {
    edges : cell list ;
    position : position option ;
  }

type move =
  {
    move_piece : piece ;
    move_from : cell ;
    move_to : cell ;
  }

type t =
  {
    top : cell ;
    bot : cell ;
    cells : cell list ;
    pieces : (position * piece) list ;
    matrix : cell array array ;
  }

let empty =
  let rec top = { edges = [l0c0 ; l0c1 ; l0c2 ; l0c3 ; l0c4 ; l0c5] ; position = None }

  and l0c0 = { edges = [top ; l1c0 ; l0c1] ; position = Some (0, 0) }
  and l0c1 = { edges = [top ; l1c1 ; l0c0 ; l0c2] ; position = Some (0, 1) }
  and l0c2 = { edges = [top ; l1c2 ; l0c1 ; l0c3] ; position = Some (0, 2) }
  and l0c3 = { edges = [top ; l1c3 ; l0c2 ; l0c4] ; position = Some (0, 3) }
  and l0c4 = { edges = [top ; l1c4 ; l0c3 ; l0c5] ; position = Some (0, 4) }
  and l0c5 = { edges = [top ; l1c5 ; l0c4] ; position = Some (0, 5) }

  and l1c0 = { edges = [l0c0 ; l2c0 ; l1c1] ; position = Some (1, 0) }
  and l1c1 = { edges = [l0c1 ; l2c1 ; l1c2 ; l1c0] ; position = Some (1, 1) }
  and l1c2 = { edges = [l0c2 ; l2c2 ; l1c3 ; l1c1] ; position = Some (1, 2) }
  and l1c3 = { edges = [l0c3 ; l2c3 ; l1c4 ; l1c2] ; position = Some (1, 3) }
  and l1c4 = { edges = [l0c4 ; l2c4 ; l1c5 ; l1c3] ; position = Some (1, 4) }
  and l1c5 = { edges = [l0c5 ; l2c5 ; l1c4] ; position = Some (1, 5) }

  and l2c0 = { edges = [l1c0 ; l3c0 ; l2c1] ; position = Some (2, 0) }
  and l2c1 = { edges = [l1c1 ; l3c1 ; l2c2 ; l2c0] ; position = Some (2, 1) }
  and l2c2 = { edges = [l1c2 ; l3c2 ; l2c3 ; l2c1] ; position = Some (2, 2) }
  and l2c3 = { edges = [l1c3 ; l3c3 ; l2c4 ; l2c2] ; position = Some (2, 3) }
  and l2c4 = { edges = [l1c4 ; l3c4 ; l2c5 ; l2c3] ; position = Some (2, 4) }
  and l2c5 = { edges = [l1c5 ; l3c5 ; l2c4] ; position = Some (2, 5) }

  and l3c0 = { edges = [l2c0 ; l4c0 ; l3c1] ; position = Some (3, 0) }
  and l3c1 = { edges = [l2c1 ; l4c1 ; l3c2 ; l3c0] ; position = Some (3, 1) }
  and l3c2 = { edges = [l2c2 ; l4c2 ; l3c3 ; l3c1] ; position = Some (3, 2) }
  and l3c3 = { edges = [l2c3 ; l4c3 ; l3c4 ; l3c2] ; position = Some (3, 3) }
  and l3c4 = { edges = [l2c4 ; l4c4 ; l3c5 ; l3c3] ; position = Some (3, 4) }
  and l3c5 = { edges = [l2c5 ; l4c5 ; l3c4] ; position = Some (3, 5) }

  and l4c0 = { edges = [l3c0 ; l5c0 ; l4c1] ; position = Some (4, 0) }
  and l4c1 = { edges = [l3c1 ; l5c1 ; l4c2 ; l4c0] ; position = Some (4, 1) }
  and l4c2 = { edges = [l3c2 ; l5c2 ; l4c3 ; l4c1] ; position = Some (4, 2) }
  and l4c3 = { edges = [l3c3 ; l5c3 ; l4c4 ; l4c2] ; position = Some (4, 3) }
  and l4c4 = { edges = [l3c4 ; l5c4 ; l4c5 ; l4c3] ; position = Some (4, 4) }
  and l4c5 = { edges = [l3c5 ; l5c5 ; l4c4] ; position = Some (4, 5) }

  and l5c0 = { edges = [bot ; l4c0 ; l5c1] ; position = Some (5, 0) }
  and l5c1 = { edges = [bot ; l4c1 ; l5c2 ; l5c0] ; position = Some (5, 1) }
  and l5c2 = { edges = [bot ; l4c2 ; l5c3 ; l5c1] ; position = Some (5, 2) }
  and l5c3 = { edges = [bot ; l4c3 ; l5c4 ; l5c2] ; position = Some (5, 3) }
  and l5c4 = { edges = [bot ; l4c4 ; l5c5 ; l5c3] ; position = Some (5, 4) }
  and l5c5 = { edges = [bot ; l4c5 ; l5c4] ; position = Some (5, 5) }

  and bot = { edges = [l5c0 ; l5c1 ; l5c2 ; l5c3 ; l5c4 ; l5c5] ; position = None } in

  let cells =
    [
      top ;
      l0c0 ; l0c1 ; l0c2 ; l0c3 ; l0c4 ; l0c5 ;
      l1c0 ; l1c1 ; l1c2 ; l1c3 ; l1c4 ; l1c5 ;
      l2c0 ; l2c1 ; l2c2 ; l2c3 ; l2c4 ; l2c5 ;
      l3c0 ; l3c1 ; l3c2 ; l3c3 ; l3c4 ; l3c5 ;
      l4c0 ; l4c1 ; l4c2 ; l4c3 ; l4c4 ; l4c5 ;
      l5c0 ; l5c1 ; l5c2 ; l5c3 ; l5c4 ; l5c5 ;
      bot
    ]
  and matrix =
    [|
      [| l0c0 ; l0c1 ; l0c2 ; l0c3 ; l0c4 ; l0c5 |] ;
      [| l1c0 ; l1c1 ; l1c2 ; l1c3 ; l1c4 ; l1c5 |] ;
      [| l2c0 ; l2c1 ; l2c2 ; l2c3 ; l2c4 ; l2c5 |] ;
      [| l3c0 ; l3c1 ; l3c2 ; l3c3 ; l3c4 ; l3c5 |] ;
      [| l4c0 ; l4c1 ; l4c2 ; l4c3 ; l4c4 ; l4c5 |] ;
      [| l5c0 ; l5c1 ; l5c2 ; l5c3 ; l5c4 ; l5c5 |] ;
    |]
  in

  {
    top = top ;
    bot = bot ;
    cells = cells ;
    pieces = [] ;
    matrix = matrix ;
  }

let piece_on ~board ~position =
  let rec scan_pieces = function
    | [] -> None
    | (p, piece)::_ when p = position -> Some piece
    | _::rest -> scan_pieces rest
  in
  scan_pieces board.pieces

let add ~board ~piece ~position =
  { board with pieces = (position, piece)::board.pieces }

let print ~board =
  let rec string_of_cell cell =
    match cell.position with
      | None -> "O"
      | Some(position) ->
          begin
            match (piece_on ~board ~position) with
              | None -> "O"
              | Some(PieceOne) -> "1"
              | Some(PieceTwo) -> "2"
              | Some(PieceThree) -> "3"
          end
  and string_of_cell_line line =
    let cell_list = Array.to_list line in
    let cell_list = List.map string_of_cell cell_list in
    String.concat " | " cell_list
  and string_of_cell_matrix matrix =
    let lines = Array.to_list matrix in
    let cell_lines = List.map (fun line -> string_of_cell_line line) lines in
    String.concat "\n---------------------\n" cell_lines
  in
  print_endline "----------O----------" ;
  print_endline (string_of_cell_matrix board.matrix) ;
  print_endline "----------O----------"
