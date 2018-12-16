open Reprocessing;

type stateT = {
  birbs: array(Birb.t),
  selectedBirbIndex: option(int),
};

let maxVel = 0.5;

let updatePoint = (env, point) =>
  point |> Body.update |> Body.wrap(env) |> Body.limitVel(maxVel);

let setup = env => {
  Env.size(~width=600, ~height=600, env);
  let birbs = Array.init(100, _i => Birb.random(env));
  {birbs, selectedBirbIndex: Some(0)};
};

let draw = (state, env) => {
  Draw.background(Utils.color(~r=55, ~g=55, ~b=55, ~a=255), env);
  Draw.strokeWeight(1, env);
  state.birbs |> Array.iter((birb: Birb.t) => Birb.drawBirb(env, birb));
  switch (state.selectedBirbIndex) {
  | Some(index) => Birb.drawCircle(env, state.birbs[index])
  | None => ()
  };
  {...state, birbs: state.birbs |> Array.map(Birb.update(env))};
};

run(~setup, ~draw, ());