open Reprocessing;

let speed = 1.0;

let neighborDistance = 100.0;

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
  a !== b && Vector.distance(a.body.pos, b.body.pos) < neighborDistance;

let getAlignment = (birb, neighbors) => {
  let averageVel =
    neighbors |> List.map(neighbor => neighbor.body.vel) |> Vector.average;
  let steering =
    averageVel |> Vector.setMag(1.0) |> Vector.sub(birb.body.vel);
  /* |> Vector.setMag(1.0); */
  steering;
};

let getSeparation = (birb, neighbors) => {
  let separation =
    neighbors
    |> List.map(neighbor => {
         let awayFromNeighbor =
           birb.body.pos |> Vector.sub(neighbor.body.pos);
         let desiredVel = awayFromNeighbor |> Vector.limit(1.0);
         let steering = desiredVel |> Vector.sub(birb.body.vel);
         let distance = Vector.distance(birb.body.pos, neighbor.body.pos);
         /* https://www.wolframalpha.com/input/?i=exponential+fit+%5B(0,+1),+(10,+0.5),+(50,+0)%5D */
         /* let magnitude = 2.71828 ** ((-0.0656449) *. distance); */
         let magnitude = 1.0 -. 0.01 *. distance;
         steering |> Vector.setMag(magnitude);
       })
    |> Vector.average;
  separation;
};

let getCohesion = (birb, neighbors) => {
  let averagePos =
    neighbors |> List.map(neighbor => neighbor.body.pos) |> Vector.average;
  let toAveragePos =
    averagePos |> Vector.sub(birb.body.pos) |> Vector.setMag(1.0);
  let steering =
    toAveragePos |> Vector.sub(birb.body.vel) |> Vector.limit(1.0);
  steering;
};

let update = (birbs, env, birb) => {
  let neighbors = birbs |> Array.to_list |> List.filter(isNeighbor(birb));
  let alignment = getAlignment(birb, neighbors);
  let separation = getSeparation(birb, neighbors);
  let cohesion = getCohesion(birb, neighbors);
  let force =
    Vector.average([
      /* alignment |> Vector.setMag(speed *. 1.0),
         separation |> Vector.setMag(speed *. 1.0),
         cohesion |> Vector.setMag(speed *. 1.0), */
      alignment,
      separation,
      cohesion,
    ]);
  let body =
    birb.body
    |> Body.addForce(force)
    |> Body.update
    |> Body.limitVel(speed)
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
    ~p1=(0.0, (-3.0)),
    ~p2=(1.5, 2.0),
    ~p3=((-1.5), 2.0),
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