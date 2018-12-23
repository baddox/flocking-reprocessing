open Reprocessing;

let birbCount = 200;

type stateT = {
  birbs: array(Birb.t),
  selectedBirbIndex: option(int),
};

let initialState = env => {
  let birbs = Array.init(birbCount, _i => Birb.random(env));
  {birbs, selectedBirbIndex: Some(0)};
};

let setup = env => {
  Env.size(~width=600, ~height=400, env);
  initialState(env);
};

let draw = (state, env) => {
  Draw.background(Utils.color(~r=55, ~g=55, ~b=55, ~a=255), env);
  Draw.strokeWeight(1, env);
  state.birbs |> Array.iter((birb: Birb.t) => Birb.draw(birb, env));
  let selectedBirb =
    switch (state.selectedBirbIndex) {
    | Some(index) => Some(state.birbs[index])
    | None => None
    };
  switch (selectedBirb) {
  | Some(selectedBirb) =>
    state.birbs
    |> Array.iter((other: Birb.t) =>
         if (Birb.isNeighbor(selectedBirb, other)) {
           Birb.draw(other, ~neighbor=true, env);
         }
       );
    Birb.draw(selectedBirb, ~selected=true, env);
  | None => ()
  };
  {
    ...state,
    birbs:
      state.birbs
      |> Array.map(birb =>
           birb |> Birb.update(state.birbs, selectedBirb, env)
         ),
  };
};

let mouseUp = (state, env) => initialState(env);

run(~setup, ~draw, ~mouseUp, ());