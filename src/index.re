open Reprocessing;

let birbCount = 300;

type stateT = {
  birbs: array(Birb.t),
  selectedBirbIndex: option(int),
};

let setup = env => {
  Env.size(~width=600, ~height=600, env);
  let birbs = Array.init(birbCount, _i => Birb.random(env));
  {birbs, selectedBirbIndex: Some(0)};
};

let draw = (state, env) => {
  Draw.background(Utils.color(~r=55, ~g=55, ~b=55, ~a=255), env);
  Draw.strokeWeight(1, env);
  state.birbs |> Array.iter((birb: Birb.t) => Birb.draw(birb, env));
  switch (state.selectedBirbIndex) {
  | Some(index) =>
    let birb = state.birbs[index];
    state.birbs
    |> Array.iter((other: Birb.t) =>
         if (Birb.isNeighbor(birb, other)) {
           Birb.draw(other, ~neighbor=true, env);
         }
       );
    Birb.draw(birb, ~selected=true, env);
    Birb.drawCircle(env, birb);
  | None => ()
  };
  {
    ...state,
    birbs:
      state.birbs |> Array.map(birb => birb |> Birb.update(state.birbs, env)),
  };
};

run(~setup, ~draw, ());