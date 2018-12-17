open Reprocessing;

type t = {
  x: float,
  y: float,
};

let create = (x, y) : t => {x, y};

let fromCoords = create;

let fromPolar = (distance, angle) : t => {
  let x = distance *. cos(angle);
  let y = distance *. sin(angle);
  fromCoords(x, y);
};

let add = (a: t, b: t) : t => create(a.x +. b.x, a.y +. b.y);

let sub = (a: t, b: t) : t => create(a.x -. b.x, a.y -. b.y);

let mult = (n: float, vec: t) : t => create(vec.x *. n, vec.y *. n);

let div = (n: float, vec: t) : t => create(vec.x /. n, vec.y /. n);

let magSq = (vec: t) => {
  let {x, y} = vec;
  x *. x +. y *. y;
};

let mag = (vec: t) => sqrt(magSq(vec));

let heading = (vec: t) => atan2(vec.x, vec.y);

let limit = (max: float, vec: t) : t => {
  let mSq = magSq(vec);
  if (mSq > max *. max) {
    vec |> div(sqrt(mSq)) |> mult(max);
  } else {
    vec;
  };
};

let normalize = (vec: t) : t => {
  let mag = mag(vec);
  mag == 0. ? vec : div(mag, vec);
};

let setMag = (n: float, vec: t) : t => vec |> normalize |> mult(n);

let limitMag = (limit: float, vec: t) : t =>
  mag(vec) > limit ? setMag(limit, vec) : vec;

let distance = (a: t, b: t) =>
  sqrt((a.x -. b.x) ** 2.0 +. (a.y -. b.y) ** 2.0);

let wrap = (env, vector) => {
  let width = Env.width(env) |> float_of_int;
  let height = Env.height(env) |> float_of_int;
  fromCoords(Util.wrap(vector.x, width), Util.wrap(vector.y, height));
};

let random = (xMin, xMax, yMin, yMax) => {
  let x = Util.random(xMin, xMax);
  let y = Util.random(yMin, yMax);
  fromCoords(x, y);
};