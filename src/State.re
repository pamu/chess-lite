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

type coord = {
  x: int,
  y: int,
};

type state = {
  playerColor: color,
  chessBoard: board,
  currSelection: option(coord),
  possibleMoves: list(coord),
};

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

let initialBoard = (playerFacingColor: color) => {
  let pieces = color => [|
    Rook(color),
    Knight(color),
    Bishop(color),
    Queen(color),
    King(color),
    Bishop(color),
    Knight(color),
    Rook(color),
  |];

  let counterColor =
    switch (playerFacingColor) {
    | White => Black
    | Black => White
    };

  let board = emptyBoard;

  let topPiecesRow = 0;
  for (col in 0 to 7) {
    fillPiece(board, (topPiecesRow, col), pieces(counterColor)[col]);
  };

  let topPawnRow = 1;
  for (col in 0 to 7) {
    fillPiece(board, (topPawnRow, col), Pawn(counterColor));
  };

  let bottomPawnRow = 6;
  for (col in 0 to 7) {
    fillPiece(board, (bottomPawnRow, col), Pawn(playerFacingColor));
  };

  let bottomPiecesRow = 7;
  for (col in 0 to 7) {
    fillPiece(
      board,
      (bottomPiecesRow, col),
      pieces(playerFacingColor)[col],
    );
  };

  board;
};

let initialState = color => {
  {
    playerColor: color,
    chessBoard: initialBoard(color),
    currSelection: None,
    possibleMoves: [],
  };
};