module Validations = {
  let withinBoard = (pos: State.coord) =>
    pos.x >= 0 && pos.x <= 7 && pos.y >= 0 && pos.y <= 7;

  let isObsticle = (board: State.board, pos: State.coord) =>
    Belt.Option.isSome(board[pos.x][pos.y].piece);

  let isOpponent = (state: State.state, pos: State.coord) => {
    switch (state.chessBoard[pos.x][pos.y].piece) {
    | Some(piece) =>
      switch (piece) {
      | King(color) => color != state.playerColor
      | Queen(color) => color != state.playerColor
      | Rook(color) => color != state.playerColor
      | Bishop(color) => color != state.playerColor
      | Knight(color) => color != state.playerColor
      | Pawn(color) => color != state.playerColor
      }
    | None => false
    };
  };

  let isFriendly = (state: State.state, x) =>
    isObsticle(state.chessBoard, x) && !isOpponent(state, x);
};

module Generators = {
  let rec generate =
          (
            startExcluded: State.coord,
            state: State.state,
            generator: State.coord => State.coord,
            until: (State.state, State.coord) => bool,
            onLast: (State.state, State.coord) => list(State.coord),
          )
          : list(State.coord) => {
    let newPos = generator(startExcluded);
    until(state, newPos)
      ? [newPos, ...generate(newPos, state, generator, until, onLast)]
      : onLast(state, newPos);
  };
};

let pawnMoves = (state: State.state, pos: State.coord): list(State.coord) => {
  open Validations;
  let twoStepsForward =
    List.filter(
      x => !isObsticle(state.chessBoard, x),
      pos.x == 6 ? [State.{...pos, x: pos.x - 2}] : [],
    );

  let straightMoves =
    List.filter(
      x => withinBoard(x) && !isObsticle(state.chessBoard, x),
      [{...pos, x: pos.x - 1}],
    );

  let cornerMoves =
    List.filter(
      x => withinBoard(x) && isOpponent(state, x),
      [
        State.{x: pos.x - 1, y: pos.y - 1},
        State.{x: pos.x - 1, y: pos.y + 1},
      ],
    );
  List.concat([straightMoves, cornerMoves, twoStepsForward]);
};

let knightMoves = (state: State.state, pos: State.coord): list(State.coord) => {
  open Validations;
  let moves = [
    State.{x: pos.x + 1, y: pos.y + 2},
    State.{x: pos.x + 1, y: pos.y - 2},
    State.{x: pos.x - 1, y: pos.y + 2},
    State.{x: pos.x - 1, y: pos.y - 2},
    State.{x: pos.x + 2, y: pos.y + 1},
    State.{x: pos.x + 2, y: pos.y - 1},
    State.{x: pos.x - 2, y: pos.y + 1},
    State.{x: pos.x - 2, y: pos.y - 1},
  ];

  moves |> List.filter(x => withinBoard(x) && !isFriendly(state, x));
};

let kingMoves = (state: State.state, pos: State.coord): list(State.coord) => {
  open Validations;
  let moves = [
    State.{x: pos.x + 1, y: pos.y + 1},
    State.{x: pos.x - 1, y: pos.y - 1},
    State.{x: pos.x + 1, y: pos.y - 1},
    State.{x: pos.x - 1, y: pos.y + 1},
    State.{...pos, x: pos.x + 1},
    State.{...pos, x: pos.x - 1},
    State.{...pos, y: pos.y + 1},
    State.{...pos, y: pos.y - 1},
  ];
  moves |> List.filter(x => withinBoard(x) && !isFriendly(state, x));
};

let bishopMoves = (state: State.state, pos: State.coord): list(State.coord) => {
  open Generators;
  open Validations;
  let bishopMovesGenerator = generator => {
    generate(
      pos,
      state,
      generator,
      (state: State.state, pos: State.coord) =>
        withinBoard(pos) && !isObsticle(state.chessBoard, pos),
      (state: State.state, pos: State.coord) => {
        Js.Console.log("last included");
        withinBoard(pos) && !isFriendly(state, pos) ? [pos] : [];
      },
    );
  };
  List.concat([
    bishopMovesGenerator(lastPos =>
      State.{x: lastPos.x - 1, y: lastPos.y - 1}
    ),
    bishopMovesGenerator(lastPos =>
      State.{x: lastPos.x + 1, y: lastPos.y + 1}
    ),
    bishopMovesGenerator(lastPos =>
      State.{x: lastPos.x + 1, y: lastPos.y - 1}
    ),
    bishopMovesGenerator(lastPos =>
      State.{x: lastPos.x - 1, y: lastPos.y + 1}
    ),
  ]);
};

let queenMoves = (state: State.state, pos: State.coord): list(State.coord) => {
  open Validations;
  open Generators;

  let bishopMovesGenerator = generator => {
    generate(
      pos,
      state,
      generator,
      (state: State.state, pos: State.coord) =>
        withinBoard(pos) && !isObsticle(state.chessBoard, pos),
      (state: State.state, pos: State.coord) =>
        withinBoard(pos) && !isFriendly(state, pos) ? [pos] : [],
    );
  };

  let bishopMoves =
    List.concat([
      bishopMovesGenerator(lastPos =>
        State.{x: lastPos.x - 1, y: lastPos.y - 1}
      ),
      bishopMovesGenerator(lastPos =>
        State.{x: lastPos.x + 1, y: lastPos.y + 1}
      ),
      bishopMovesGenerator(lastPos =>
        State.{x: lastPos.x + 1, y: lastPos.y - 1}
      ),
      bishopMovesGenerator(lastPos =>
        State.{x: lastPos.x - 1, y: lastPos.y + 1}
      ),
    ]);

  let rookMovesGenerator = generator => {
    generate(
      pos,
      state,
      generator,
      (state: State.state, pos: State.coord) =>
        withinBoard(pos) && !isObsticle(state.chessBoard, pos),
      (state: State.state, pos: State.coord) =>
        withinBoard(pos) && !isFriendly(state, pos) ? [pos] : [],
    );
  };
  let rookMoves =
    List.concat([
      rookMovesGenerator(lastPos => {...lastPos, x: lastPos.x + 1}),
      rookMovesGenerator(lastPos => {...lastPos, x: lastPos.x - 1}),
      rookMovesGenerator(lastPos => {...lastPos, y: lastPos.y + 1}),
      rookMovesGenerator(lastPos => {...lastPos, y: lastPos.y - 1}),
    ]);

  List.concat([bishopMoves, rookMoves]);
};

let rookMoves = (state: State.state, pos: State.coord): list(State.coord) => {
  open Validations;
  open Generators;
  let rookMovesGenerator = generator => {
    generate(
      pos,
      state,
      generator,
      (state: State.state, pos: State.coord) =>
        withinBoard(pos) && !isObsticle(state.chessBoard, pos),
      (state: State.state, pos: State.coord) =>
        withinBoard(pos) && !isFriendly(state, pos) ? [pos] : [],
    );
  };
  List.concat([
    rookMovesGenerator(lastPos => {...lastPos, x: lastPos.x + 1}),
    rookMovesGenerator(lastPos => {...lastPos, x: lastPos.x - 1}),
    rookMovesGenerator(lastPos => {...lastPos, y: lastPos.y + 1}),
    rookMovesGenerator(lastPos => {...lastPos, y: lastPos.y - 1}),
  ]);
};

let nextMoves = (state: State.state, pos: State.coord) => {
  let cell = state.chessBoard[pos.x][pos.y];
  switch (cell.piece) {
  | Some(piece) =>
    switch (piece) {
    | King(color) => color != state.playerColor ? [] : kingMoves(state, pos)
    | Queen(color) =>
      color != state.playerColor ? [] : queenMoves(state, pos)
    | Rook(color) => color != state.playerColor ? [] : rookMoves(state, pos)
    | Bishop(color) =>
      color != state.playerColor ? [] : bishopMoves(state, pos)
    | Knight(color) =>
      color != state.playerColor ? [] : knightMoves(state, pos)
    | Pawn(color) => color != state.playerColor ? [] : pawnMoves(state, pos)
    }
  | None => []
  };
};