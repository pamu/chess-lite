type color =
  | White
  | Black;

type piece =
  | King(color)
  | Queen(color)
  | Rook(color)
  | Bishop(color)
  | Knight(color)
  | Pawn(color);

type cell = {
  color,
  piece: option(piece),
};

type board = array(array(cell));

let emptyBoard =
  Array.init(8, row =>
    Array.init(8, col =>
      {color: (row + col) mod 2 == 0 ? White : Black, piece: None}
    )
  );

let fillPiece = (board, xy, piece) =>
  board[fst(xy)][snd(xy)] = {
    ...board[fst(xy)][snd(xy)],
    piece: Some(piece),
  };

let initialBoard: board = {
  let pieces = color => [|
    Rook(color),
    Knight(color),
    Bishop(color),
    King(color),
    Queen(color),
    Bishop(color),
    Knight(color),
    Rook(color),
  |];

  let board = emptyBoard;

  let topPiecesRow = 0;
  for (col in 0 to 7) {
    fillPiece(board, (topPiecesRow, col), pieces(Black)[col]);
  };

  let topPawnRow = 1;
  for (col in 0 to 7) {
    fillPiece(board, (topPawnRow, col), Pawn(Black));
  };

  let bottomPawnRow = 6;
  for (col in 0 to 7) {
    fillPiece(board, (bottomPawnRow, col), Pawn(White));
  };

  let bottomPiecesRow = 7;
  for (col in 0 to 7) {
    fillPiece(board, (bottomPiecesRow, col), pieces(White)[col]);
  };
  board;
};

let colorCode =
  fun
  | White => "#fff"
  | Black => "#000";

let pieceCodeAndColor =
  fun
  | King(color) => (colorCode(color), 0xf43f)
  | Queen(color) => (colorCode(color), 0xf445)
  | Bishop(color) => (colorCode(color), 0xf43a)
  | Knight(color) => (colorCode(color), 0xf441)
  | Rook(color) => (colorCode(color), 0xf447)
  | Pawn(color) => (colorCode(color), 0xf443);

let printPiece = (piece: piece) => {
  let (color, code) = pieceCodeAndColor(piece);
  <i className="fas" style={ReactDOMRe.Style.make(~color, ())}>
    {React.string(Js.String.fromCharCode(code))}
  </i>;
};

let printCell = cell => {
  let classname = cell.color == White ? "white-cell" : "black-cell";
  <div className=classname>
    {switch (cell.piece) {
     | Some(piece) => printPiece(piece)
     | None => <i />
     }}
  </div>;
};

let printRow = row => {
  row |> Array.map(printCell) |> React.array;
};

let printBoard = board => {
  <div className="grid"> {board |> Array.map(printRow) |> React.array} </div>;
};

[@react.component]
let make = (~board) => {
  <main> <div className="container"> {printBoard(board)} </div> </main>;
};