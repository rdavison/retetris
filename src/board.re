open! Import;

type t = Matrix.t(option(Piece.color));

let make = (~rows, ~cols) => Matrix.init(rows, cols, ~f=(_, _) => None);

let width = t => Matrix.dimens(t) |> snd;

let height = t => Matrix.dimens(t) |> fst;

let dimens = t => Matrix.dimens(t);

let hasCollision = (piece, t) => {
  let pos: Piece.point = Piece.pos(piece);
  let (rows, cols) = piece |> Piece.shape |> Matrix.dimens;
  let shadowHasCollision = () => {
    let rec checkShadow = (r, c) =>
      if (r < rows && c < cols) {
        let pieceCell = Option.join(Piece.get(piece, ~r, ~c));
        let boardCell =
          Option.join(Matrix.get(t, ~r=pos.row + r, ~c=pos.col + c));
        switch (pieceCell, boardCell) {
        | (Some(_), Some(_)) => true
        | (_, _) => checkShadow(r, c + 1)
        };
      } else if (r < rows && c >= cols) {
        checkShadow(r + 1, 0);
      } else {
        false;
      };
    checkShadow(0, 0);
  };
  pos.row < 0
  || pos.row
  + rows
  - 1 >= height(t)
  || pos.col < 0
  || pos.col
  + cols
  - 1 >= width(t)
  || shadowHasCollision();
};

let writePieceToBoard = (piece, t) => {
  let {row, col}: Piece.point = Piece.pos(piece);
  let (rows, cols) = Piece.shape(piece) |> Matrix.dimens;
  for (r in 0 to rows - 1) {
    for (c in 0 to cols - 1) {
      let data = piece |> Piece.shape |> Matrix.get(~r, ~c) |> Option.join;
      switch (data) {
      | None => ()
      | Some(_) as data => Matrix.set(t, ~r=row + r, ~c=col + c, ~data)
      };
    };
  };
};

let freeze = (piece, t) =>
  if (hasCollision(piece, t)) {
    Result.Error(`Collision);
  } else {
    writePieceToBoard(piece, t);
    Result.Ok();
  };

let get = (t, ~r, ~c) => Matrix.get(t, ~r, ~c);

let set = (t, ~r, ~c, ~data) => Matrix.set(t, ~r, ~c, ~data);
