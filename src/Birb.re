open Reprocessing;

type t = {body: Body.t};

let create = body => {body: body};

let random = env => {
  let pos =
    Vector.random(
      0.0,
      float_of_int(Env.width(env)),
      0.0,
      float_of_int(Env.height(env)),
    );
  let vel = Vector.fromPolar(1.0, Util.random(0.0, Constants.two_pi));
  /* let vel = Vector.fromPolar(1.0, -. (Constants.half_pi /. 2.0)); */
  let body = Body.create(~pos, ~vel, ());
  create(body);
};

let isNeighbor = (a, b) =>
  a !== b && Vector.distance(a.body.pos, b.body.pos) < 50.0;

let averageHeading = birbs => {
  let headings =
    birbs
    |> Array.map(birb => birb.body.vel |> Vector.heading)
    |> Array.to_list;
  let average =
    (headings |> List.fold_left((+.), 0.0))
    /. (List.length(headings) |> float_of_int);
  average;
};

let averagePosition = birbs => {
  let positions = birbs |> Array.map(birb => birb.body.pos) |> Array.to_list;
  let average =
    positions
    |> List.fold_left(Vector.add, Vector.create(0.0, 0.0))
    |> Vector.div(List.length(positions) |> float_of_int);
  average;
};

let update = (birbs, env, birb) => {
  let neighbors = birbs |> Array.to_list |> List.filter(isNeighbor(birb));
  let headingForce = Vector.fromPolar(0.1, averageHeading(birbs));
  let positionForce = Vector.sub(averagePosition(birbs), birb.body.pos);
  let body =
    birb.body
    |> Body.addForce(headingForce)
    |> Body.addForce(positionForce)
    |> Body.update
    |> Body.limitVel(1.0)
    |> Body.wrap(env);
  {...birb, body};
};

let draw = (birb, ~hovered=false, ~selected=false, ~neighbor=false, env) => {
  Draw.pushMatrix(env);
  Draw.translate(~x=birb.body.pos.x, ~y=birb.body.pos.y, env);
  Draw.rotate(-. Vector.heading(birb.body.vel) +. Constants.pi, env);
  /* Draw.scale(~x=2.0, ~y=2.0, env); */
  /* Draw.pixelf(~pos=(0.0, 0.0), ~color=Constants.white, env); */
  if (selected) {
    Draw.fill(Constants.white, env);
  };
  if (neighbor) {
    Draw.stroke(Constants.green, env);
  };
  Draw.trianglef(
    ~p1=(0.0, (-5.0)),
    ~p2=(3.0, 5.0),
    ~p3=((-3.0), 5.0),
    env,
  );
  Draw.popMatrix(env);
};

let drawCircle = (env, birb) => {
  Draw.noFill(env);
  Draw.stroke(Constants.white, env);
  Draw.ellipsef(
    ~center=(birb.body.pos.x, birb.body.pos.y),
    ~radx=10.0,
    ~rady=10.0,
    env,
  );
};