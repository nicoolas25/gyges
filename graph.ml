type piece = PieceOne | PieceTwo | PieceThree

type cell =
  {
    edges : cell list ;
  }

type board =
  {
    cells : cell list ;
    matrix : cell array array ;
    top : cell ;
    bot : cell ;
  }

let make_board () =
  let rec top = { edges = [l0c0 ; l0c1 ; l0c2 ; l0c3 ; l0c4 ; l0c5] }

  and l0c0 = { edges = [top ; l1c0 ; l0c1] }
  and l0c1 = { edges = [top ; l1c1 ; l0c0 ; l0c2] }
  and l0c2 = { edges = [top ; l1c2 ; l0c1 ; l0c3] }
  and l0c3 = { edges = [top ; l1c3 ; l0c2 ; l0c4] }
  and l0c4 = { edges = [top ; l1c4 ; l0c3 ; l0c5] }
  and l0c5 = { edges = [top ; l1c5 ; l0c4] }

  and l1c0 = { edges = [l0c0 ; l2c0 ; l1c1] }
  and l1c1 = { edges = [l0c1 ; l2c1 ; l1c2 ; l1c0] }
  and l1c2 = { edges = [l0c2 ; l2c2 ; l1c3 ; l1c1] }
  and l1c3 = { edges = [l0c3 ; l2c3 ; l1c4 ; l1c2] }
  and l1c4 = { edges = [l0c4 ; l2c4 ; l1c5 ; l1c3] }
  and l1c5 = { edges = [l0c5 ; l2c5 ; l1c4] }

  and l2c0 = { edges = [l1c0 ; l3c0 ; l2c1] }
  and l2c1 = { edges = [l1c1 ; l3c1 ; l2c2 ; l2c0] }
  and l2c2 = { edges = [l1c2 ; l3c2 ; l2c3 ; l2c1] }
  and l2c3 = { edges = [l1c3 ; l3c3 ; l2c4 ; l2c2] }
  and l2c4 = { edges = [l1c4 ; l3c4 ; l2c5 ; l2c3] }
  and l2c5 = { edges = [l1c5 ; l3c5 ; l2c4] }

  and l3c0 = { edges = [l2c0 ; l4c0 ; l3c1] }
  and l3c1 = { edges = [l2c1 ; l4c1 ; l3c2 ; l3c0] }
  and l3c2 = { edges = [l2c2 ; l4c2 ; l3c3 ; l3c1] }
  and l3c3 = { edges = [l2c3 ; l4c3 ; l3c4 ; l3c2] }
  and l3c4 = { edges = [l2c4 ; l4c4 ; l3c5 ; l3c3] }
  and l3c5 = { edges = [l2c5 ; l4c5 ; l3c4] }

  and l4c0 = { edges = [l3c0 ; l5c0 ; l4c1] }
  and l4c1 = { edges = [l3c1 ; l5c1 ; l4c2 ; l4c0] }
  and l4c2 = { edges = [l3c2 ; l5c2 ; l4c3 ; l4c1] }
  and l4c3 = { edges = [l3c3 ; l5c3 ; l4c4 ; l4c2] }
  and l4c4 = { edges = [l3c4 ; l5c4 ; l4c5 ; l4c3] }
  and l4c5 = { edges = [l3c5 ; l5c5 ; l4c4] }

  and l5c0 = { edges = [bot ; l4c0 ; l5c1] }
  and l5c1 = { edges = [bot ; l4c1 ; l5c2 ; l5c0] }
  and l5c2 = { edges = [bot ; l4c2 ; l5c3 ; l5c1] }
  and l5c3 = { edges = [bot ; l4c3 ; l5c4 ; l5c2] }
  and l5c4 = { edges = [bot ; l4c4 ; l5c5 ; l5c3] }
  and l5c5 = { edges = [bot ; l4c5 ; l5c4] }

  and bot = { edges = [l5c0 ; l5c1 ; l5c2 ; l5c3 ; l5c4 ; l5c5] } in

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
    cells = cells ;
    matrix = matrix ;
    top = top ;
    bot = bot ;
  }

let print_board board =
  let rec string_of_cell =
    fun _ -> "O"
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

let () =
  let board = make_board () in
  print_board board ;
  print_endline "It works!"
