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
  {playerColor: color, chessBoard: initialBoard(color), currSelection: None};
};

type action =
  | Reset
  | TapCell(coord);

let reducer = (state, action) =>
  switch (action) {
  | Reset => initialState(state.playerColor)
  | TapCell(xy) =>
    Js.Console.log(xy);
    let newState = {...state, currSelection: Some(xy)};
    Js.Console.log(newState);
    newState;
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

let colorClass = color => color == White ? "white" : "black";

let pieceExists = (board, selectedCoord) =>
  Belt.Option.isSome(board[selectedCoord.x][selectedCoord.y].piece);

let hightlightedClass = (currCellCoord, state) => {
  switch (state.currSelection) {
  | Some(coord) =>
    currCellCoord == coord && pieceExists(state.chessBoard, coord)
      ? "selected" : ""
  | None => ""
  };
};

[@react.component]
let make = (~playerColor) => {
  let (state, dispatch) =
    React.useReducer(reducer, initialState(playerColor));
  <div className="container">
    <div className="grid">
      {state.chessBoard
       |> Array.mapi((rowIndex, row) => {
            row
            |> Array.mapi((colIndex, cell) => {
                 <div
                   onClick={_ =>
                     dispatch(TapCell({x: rowIndex, y: colIndex}))
                   }
                   key={string_of_int(colIndex)}
                   className={
                     "cell "
                     ++ colorClass(cell.color)
                     ++ " "
                     ++ hightlightedClass({x: rowIndex, y: colIndex}, state)
                   }>
                   {switch (cell.piece) {
                    | Some(piece) =>
                      let (color, code) = pieceCodeAndColor(piece);
                      <i
                        className="fas"
                        style={ReactDOMRe.Style.make(~color, ())}>
                        {React.string(Js.String.fromCharCode(code))}
                      </i>;
                    | None => <i />
                    }}
                 </div>
               })
            |> React.array
          })
       |> React.array}
    </div>
  </div>;
};