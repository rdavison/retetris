open Reprocessing;

open! Import;

type view = {
  cellWidth: int,
  cellHeight: int,
  offset: int,
  background: [
    | `Red
    | `Orange
    | `Yellow
    | `Green
    | `Blue
    | `Purple
    | `White
    | `Gray
    | `DarkGray
    | `Black
  ],
};

let view = {cellWidth: 25, cellHeight: 25, offset: 1, background: `DarkGray};

let rec colorOfEnum = (cell, state) =>
  switch (cell) {
  | None =>
    let low = colorOfEnum(Some(`Gray), state);
    let high = colorOfEnum(Some(`Black), state);
    let speed = Game.speed(state);
    let value = clock => abs_float(clock /. speed);
    let (low, high, value) =
      if (Game.flipFlop(state)) {
        (low, high, value(Game.clock(state)));
      } else {
        (high, low, value((-1.0) *. Game.clock(state)));
      };
    Utils.lerpColor(~low, ~high, ~value);
  | Some(color) =>
    switch (color) {
    | `Red => Utils.color(~r=255, ~g=121, ~b=198, ~a=255)
    | `Orange => Utils.color(~r=255, ~g=184, ~b=108, ~a=255)
    | `Yellow => Utils.color(~r=241, ~g=250, ~b=140, ~a=255)
    | `Green => Utils.color(~r=80, ~g=250, ~b=123, ~a=255)
    | `Blue => Utils.color(~r=139, ~g=233, ~b=253, ~a=255)
    | `Purple => Utils.color(~r=189, ~g=147, ~b=249, ~a=255)
    | `White => Utils.color(~r=248, ~g=248, ~b=248, ~a=255)
    | `Gray => Utils.color(~r=68, ~g=71, ~b=90, ~a=160)
    | `DarkGray => Utils.color(~r=68, ~g=71, ~b=90, ~a=64)
    | `Black => Utils.color(~r=40, ~g=42, ~b=54, ~a=255)
    }
  };

let setup = env => {
  let game = Game.make(~rows=20, ~cols=10);
  let board = Game.board(game);
  let height = {
    let boardHeight = Board.height(board);
    view.offset + boardHeight * (view.cellHeight + view.offset);
  };
  let width = {
    let w = Board.width(board);
    view.offset + w * (view.cellWidth + view.offset);
  };
  Env.size(~width, ~height, env);
  game;
};

let renderBoard = (state, env) => {
  let board = Game.board(state);
  let boardWidth = Board.width(board);
  let boardHeight = Board.height(board);
  let activePiece = Game.activePiece(state);
  for (r in 0 to boardHeight - 1) {
    for (c in 0 to boardWidth - 1) {
      let activeCell: option(Piece.color) =
        Option.bind(
          activePiece,
          piece => {
            let {row, col}: Piece.point = Piece.pos(piece);
            Option.join(Piece.get(piece, ~r=r - row, ~c=c - col));
          },
        );
      let boardCell = Option.join(Matrix.get(board, ~r, ~c));
      let cell =
        switch (activeCell, boardCell) {
        | (None, None) => None
        | (Some(x), None) => Some(x)
        | (None, Some(x)) => Some(x)
        | (Some(x), Some(_)) => Some(x)
        };
      Draw.fill(colorOfEnum(cell, state), env);
      let pos = (
        view.offset + c * (view.cellWidth + view.offset),
        view.offset + r * (view.cellHeight + view.offset),
      );
      Draw.rect(~pos, ~width=view.cellWidth, ~height=view.cellHeight, env);
    };
  };
};

let updateClock = (state, env) : Game.state => {
  let delta = Env.deltaTime(env);
  let clock = Game.clock(state) +. delta;
  let speed = Game.speed(state);
  if (clock > speed) {
    {
      ...state,
      clock: clock -. speed,
      step: true,
      flipFlop: ! Game.flipFlop(state),
    };
  } else {
    {...state, clock, step: false};
  };
};

let runStep = (state, _env) => {
  let board = Game.board(state);
  switch (Game.activePiece(state)) {
  | None =>
    let newPiece = Game.makeRandomPiece(board);
    Result.Ok(Game.setActivePiece(Some(newPiece), state));
  | Some(activePiece) =>
    let newPiece = Piece.move(`Down, activePiece);
    if (Board.hasCollision(newPiece, board)) {
      Result.map(
        Board.freeze(activePiece, board),
        () => {
          let newPiece = Game.makeRandomPiece(board);
          state
          |> Game.setActivePiece(Some(newPiece))
          |> Game.clearCompleteRows;
        },
      );
    } else {
      Result.Ok(Game.setActivePiece(Some(newPiece), state));
    };
  };
};

let drawPlay = (state, env) => {
  Draw.background(colorOfEnum(Some(view.background), state), env);
  let state =
    if (Game.step(state)) {
      switch (runStep(state, env)) {
      | Result.Ok(state) => state
      | Result.Error(`Collision) => Game.gameOver(state)
      };
    } else {
      state;
    };
  renderBoard(state, env);
  updateClock(state, env);
};

let drawGameOver = (state, env) => {
  Draw.background(colorOfEnum(Some(`Gray), state), env);
  let msg = Printf.sprintf("Score: %d", Game.score(state));
  let textWidth = Draw.textWidth(~body=msg, env);
  let x = Env.width(env) / 2 - textWidth / 2;
  let y = Env.height(env) / 2;
  Draw.text(~body=msg, ~pos=(x, y), env);
  state;
};

let draw = (state, env) =>
  switch (Game.screen(state)) {
  | `Play => drawPlay(state, env)
  | `GameOver => drawGameOver(state, env)
  };

let keyTyped = (state, env) =>
  switch (Env.keyCode(env)) {
  | Up => Game.rotateActivePiece(state, `Left)
  | Left => Game.moveActivePiece(state, `Left)
  | Right => Game.moveActivePiece(state, `Right)
  | Down => Game.moveActivePiece(state, `Down)
  | _ => state
  };

run(~setup, ~draw, ~keyTyped, ());
