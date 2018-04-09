open! Import;

type t('a) = {
  cells: array(array('a)),
  rows: int,
  cols: int,
};

let init = (r, c, ~f) => {
  let cells = Array.init(r, ~f=rth => Array.init(c, ~f=cth => f(rth, cth)));
  {cells, rows: r, cols: c};
};

let dimens = ({rows, cols, _}) => (rows, cols);

let cells = ({cells, _}) => cells;

let get = ({cells, rows, cols}, ~r, ~c) =>
  if (r < rows && c < cols && r >= 0 && c >= 0) {
    Some(cells[r][c]);
  } else {
    None;
  };

let get_exn = (t, ~r, ~c) =>
  switch (get(t, ~r, ~c)) {
  | Some(x) => x
  | None => failwith(Printf.sprintf("index out of bounds: (%d, %d)", r, c))
  };

let set = (t, ~r, ~c, ~data) => cells(t)[r][c] = data;

let transpose = t => {
  let (rows, cols) = dimens(t) |> (((a, b)) => (b, a));
  init(rows, cols, ~f=(r, c) => cells(t)[c][r]);
};

let flip = t => {
  let (rows, cols) = dimens(t);
  init(rows, cols, ~f=(r, c) => cells(t)[r][cols - c - 1]);
};

let rec rotate = (dir, t) =>
  switch (dir) {
  | `Right => transpose(t) |> flip
  | `Left =>
    let f = rotate(`Right);
    f(f(f(t)));
  };
