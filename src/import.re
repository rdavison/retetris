module Array = ArrayLabels;

module List = ListLabels;

module Option = {
  type t('a) = option('a);
  let join =
    fun
    | None => None
    | Some(x) => x;
  let map = (x, f) =>
    switch (x) {
    | None => None
    | Some(x) => Some(f(x))
    };
  let bind = (x, f) => join(map(x, f));
};

module Result = {
  type t('a, 'e) =
    | Ok('a)
    | Error('e);
  let map = (x, f) =>
    switch (x) {
    | Error(e) => Error(e)
    | Ok(x) => Ok(f(x))
    };
  let bind = (x, f) =>
    switch (x) {
    | Error(e) => Error(e)
    | Ok(x) => f(x)
    };
};
