open State;
open Chess;

type action =
  | Reset
  | TapCell(coord);

let reducer = (state, action) =>
  switch (action) {
  | Reset => initialState(state.playerColor)
  | TapCell(xy) => {
      ...state,
      currSelection: Some(xy),
      possibleMoves: nextMoves(state, xy),
    }
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

let selectedClass = (currCellCoord, state) => {
  switch (state.currSelection) {
  | Some(coord) =>
    currCellCoord == coord && pieceExists(state.chessBoard, coord)
      ? "selected" : ""
  | None => ""
  };
};

let highlightedClass = (currCellCoord, state) => {
  Js.Console.log("possible movies:");
  Js.Console.log(state.possibleMoves);
  let highlight = state.possibleMoves |> List.exists(x => x == currCellCoord);
  switch (
    highlight,
    Validations.isObsticle(state.chessBoard, currCellCoord)
    && Validations.isOpponent(state, currCellCoord),
  ) {
  | (true, true) => "highlighted-red"
  | (true, _) => "highlighted"
  | _ => ""
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
                     ++ selectedClass({x: rowIndex, y: colIndex}, state)
                     ++ " "
                     ++ highlightedClass({x: rowIndex, y: colIndex}, state)
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