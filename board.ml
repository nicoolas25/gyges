type piece = PieceOne | PieceTwo | PieceThree

type t =
  {
    top : Cell.t ;
    bot : Cell.t ;
    cells : Cell.t list ;
    pieces : (Cell.position * piece) list ;
    matrix : Cell.t array array ;
  }

exception NoPieceFoundAt of Cell.position

let empty =
  let cells, matrix = Cell.empty_cells_and_matrix in
  {
    top = List.find (fun (c:Cell.t) -> c.position = Cell.Top) cells ;
    bot = List.find (fun (c:Cell.t) -> c.position = Cell.Bot) cells ;
    cells = cells ;
    pieces = [] ;
    matrix = matrix ;
  }

let piece_at ~board ~position =
  let rec scan_pieces = function
    | [] -> None
    | (p, piece)::_ when p = position -> Some piece
    | _::rest -> scan_pieces rest
  in
  scan_pieces board.pieces

let is_empty ~board ~position =
  None = (piece_at ~board ~position)

let cell_at ~board ~position =
  match position with
    | Cell.Top -> board.top
    | Cell.Bot -> board.bot
    | Cell.Matrix (i, j) -> board.matrix.(i).(j)

let add ~board ~piece ~position =
  { board with pieces = (position, piece)::board.pieces }

let move ~board ~piece ~start ~stop =
  let rec replace found result = function
    | [] -> if found then result else raise (NoPieceFoundAt start)
    | p::rest when p = (start, piece) -> replace true ((stop, piece)::result) rest
    | p::rest -> replace found (p::result) rest
  in
  let new_pieces = replace false [] board.pieces in
  { board with pieces = new_pieces }

let print ~board ~highlight =
  let rec string_of_cell cell =
    if List.exists (Cell.equals cell) highlight then "_"
    else match cell.position with
      | Cell.Top | Cell.Bot -> "O"
      | position ->
          begin
            match (piece_at ~board ~position) with
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
  if List.exists (Cell.equals board.top) highlight
  then print_endline "--------- _ ---------"
  else print_endline "--------- O ---------" ;

  print_endline (string_of_cell_matrix board.matrix) ;

  if List.exists (Cell.equals board.bot) highlight
  then print_endline "--------- _ ---------"
  else print_endline "--------- O ---------"
