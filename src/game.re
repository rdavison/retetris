open! Import;

type state = {
  board: Board.t,
  clock: float,
  step: bool,
  activePiece: option(Piece.t),
  score: int,
  screen: [ | `Play | `GameOver],
  speed: float,
  flipFlop: bool,
};

let makeRandomPiece = board => {
  let pos: Piece.point = {row: 0, col: Board.width(board) / 2 - 1};
  let piece = {
    let shape =
      switch (Random.int(8)) {
      | 0 => `L
      | 1 => `J
      | 2 => `O
      | 3 => `T
      | 4 => `J
      | 5 => `S
      | 6 => `Z
      | 7 => `I
      | _ => failwith("Bug!")
      };
    ref(Piece.make(shape, ~pos));
  };
  let turns = Random.int(4);
  for (_ in 0 to turns) {
    piece := Piece.rotate(`Right, piece^);
  };
  piece^;
};

let make = (~rows, ~cols) => {
  let board = Board.make(~rows, ~cols);
  {
    board,
    clock: 0.0,
    step: false,
    activePiece: Some(makeRandomPiece(board)),
    score: 0,
    screen: `Play,
    speed: 0.5,
    flipFlop: true,
  };
};

let board = ({board, _}) => board;

let clock = ({clock, _}) => clock;

let step = ({step, _}) => step;

let activePiece = ({activePiece, _}) => activePiece;

let setActivePiece = (piece, t) => {...t, activePiece: piece};

let clearActivePiece = t => {...t, activePiece: None};

let replaceBoard = (board, t) => {...t, board};

let moveActivePiece = (state, dir) => {
  let activePiece = activePiece(state);
  switch (activePiece) {
  | None => state
  | Some(activePiece) =>
    let newPiece = Piece.move(dir, activePiece);
    let board = board(state);
    if (Board.hasCollision(newPiece, board)) {
      state;
    } else {
      setActivePiece(Some(newPiece), state);
    };
  };
};

let rotateActivePiece = (state, dir) => {
  let activePiece = activePiece(state);
  switch (activePiece) {
  | None => state
  | Some(activePiece) =>
    let newPiece = Piece.rotate(dir, activePiece);
    let board = board(state);
    if (Board.hasCollision(newPiece, board)) {
      /* TODO: this is too strong of a policy */
      state;
    } else {
      setActivePiece(Some(newPiece), state);
    };
  };
};

let rowIsComplete = (board, row) => {
  let width = Board.width(board);
  let rec loop = c =>
    if (c < width) {
      switch (Option.join(Board.get(board, ~r=row, ~c))) {
      | None => false
      | Some(_) => loop(c + 1)
      };
    } else {
      true;
    };
  loop(0);
};

let score = ({score, _}) => score;

let updateScore = (delta, t) => {...t, score: t.score + delta};

let clearCompleteRows = state => {
  let oldBoard = board(state);
  let (rows, cols) = Board.dimens(oldBoard);
  let newBoard = Board.make(~rows, ~cols);
  let oldBoardR = ref(Board.height(oldBoard) - 1);
  let newBoardR = ref(oldBoardR^);
  let rowsCleared = ref(0);
  while (newBoardR^ >= 0) {
    while (oldBoardR^ >= 0 && rowIsComplete(oldBoard, oldBoardR^)) {
      decr(oldBoardR);
      incr(rowsCleared);
    };
    for (c in 0 to Board.width(newBoard) - 1) {
      let data =
        if (oldBoardR^ >= 0) {
          Option.join(Board.get(oldBoard, ~r=oldBoardR^, ~c));
        } else {
          None;
        };
      Board.set(newBoard, ~r=newBoardR^, ~c, ~data);
    };
    decr(newBoardR);
    decr(oldBoardR);
  };
  state |> replaceBoard(newBoard) |> updateScore(rowsCleared^);
};

let screen = ({screen, _}) => screen;

let gameOver = state => {...state, screen: `GameOver};

let speed = ({speed, _}) => speed;

let flipFlop = ({flipFlop, _}) => flipFlop;
