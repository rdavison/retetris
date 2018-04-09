type point = {
  row: int,
  col: int,
};

type color = [
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
];

type t = {
  shape: Matrix.t(option(color)),
  pos: point,
};

let pos = ({pos, _}) => pos;

let shape = ({shape, _}) => shape;

let move = (dir, t) => {
  let {row, col} = pos(t);
  switch (dir) {
  | `Down => {
      ...t,
      pos: {
        row: row + 1,
        col,
      },
    }
  | `Left => {
      ...t,
      pos: {
        row,
        col: col - 1,
      },
    }
  | `Right => {
      ...t,
      pos: {
        row,
        col: col + 1,
      },
    }
  };
};

let rotate = (dir, t) => {...t, shape: Matrix.rotate(dir, shape(t))};

let get = (t, ~r, ~c) => Matrix.get(t.shape, ~r, ~c);

let make = (~pos={row: 0, col: 0}) =>
  fun
  | `L => {
      pos,
      shape:
        Matrix.init(3, 2, ~f=(r, c) =>
          if (r < 2 && c < 1 || r == 2) {
            Some(`Red);
          } else {
            None;
          }
        ),
    }
  | `J => {
      pos,
      shape:
        Matrix.init(3, 2, ~f=(r, c) =>
          if (r < 2 && c > 0 || r == 2) {
            Some(`Orange);
          } else {
            None;
          }
        ),
    }
  | `O => {pos, shape: Matrix.init(2, 2, ~f=(_, _) => Some(`Yellow))}
  | `T => {
      pos,
      shape:
        Matrix.init(2, 3, ~f=(r, c) =>
          if (r == 0 || c == 1) {
            Some(`Green);
          } else {
            None;
          }
        ),
    }
  | `S => {
      pos,
      shape:
        Matrix.init(2, 3, ~f=(r, c) =>
          if (r == 0 && c == 0 || r == 1 && c == 2) {
            None;
          } else {
            Some(`Blue);
          }
        ),
    }
  | `Z => {
      pos,
      shape:
        Matrix.init(2, 3, ~f=(r, c) =>
          if (r == 0 && c == 2 || r == 1 && c == 0) {
            None;
          } else {
            Some(`Purple);
          }
        ),
    }
  | `I => {pos, shape: Matrix.init(4, 1, ~f=(_, _) => Some(`White))};
