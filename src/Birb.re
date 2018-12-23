open Reprocessing;

let maxSpeed = 4.0;

let minSpeed = 2.0;

let maxForce = 0.1;

let neighborDistance = 50.0;

let white = Utils.color(255, 255, 255, 128);

let green = Utils.color(0, 255, 0, 64);

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
  let vel =
    Vector.fromPolar(
      Util.random(minSpeed, maxSpeed),
      Util.random(0.0, Constants.two_pi),
    );
  /* let vel = Vector.fromPolar(1.0, -. (Constants.half_pi /. 2.0)); */
  let body = Body.create(~pos, ~vel, ());
  create(body);
};

let isNeighbor = (a, b) => {
  let distance = Vector.distance(a.body.pos, b.body.pos);
  a != b && distance < neighborDistance;
};

let getAlignment = (birb, neighbors) =>
  if (neighbors |> List.length == 0) {
    Vector.create(0.0, 0.0);
  } else {
    let averageVel =
      neighbors
      |> List.filter(neighbor =>
           Vector.distance(birb.body.pos, neighbor.body.pos) < 25.0
         )
      |> List.map(neighbor => neighbor.body.vel)
      |> Vector.average;
    let desiredVel = averageVel |> Vector.setMag(maxSpeed);
    let steering = Vector.sub((desiredVel, birb.body.vel));
    steering |> Vector.limitMag(maxForce);
  };

let getSeparation = (birb, neighbors) => {
  let separation =
    neighbors
    |> List.map(neighbor => {
         let distance =
           Vector.distance(birb.body.pos, neighbor.body.pos) +. 0.01;
         if (distance < 10.0) {
           let awayFromNeighbor =
             Vector.sub((birb.body.pos, neighbor.body.pos));
           let desiredVel =
             awayFromNeighbor |> Vector.div(distance *. distance);
           desiredVel;
         } else {
           Vector.create(0.0, 0.0);
         };
       })
    |> Vector.average;
  let desiredVel = separation |> Vector.setMag(maxSpeed);
  let steering = Vector.sub((desiredVel, birb.body.vel));
  steering |> Vector.limitMag(maxForce);
};

let getCohesion = (birb, neighbors) => {
  let averagePos =
    neighbors |> List.map(neighbor => neighbor.body.pos) |> Vector.average;
  let toAveragePos = Vector.sub((averagePos, birb.body.pos));
  let desiredVel = toAveragePos |> Vector.setMag(maxSpeed);
  let steering = Vector.sub((desiredVel, birb.body.vel));
  steering |> Vector.limitMag(maxForce *. 0.5);
};

let update = (birbs, selectedBirb, env, birb) =>
  switch (selectedBirb) {
  | Some(b) =>
    let neighbors = birbs |> Array.to_list |> List.filter(isNeighbor(birb));
    let alignment = getAlignment(birb, neighbors);
    let separation = getSeparation(birb, neighbors);
    let cohesion = getCohesion(birb, neighbors);
    let force =
      birb == b || true ?
        Vector.sum([
          Vector.create(0.0, 0.0),
          /* alignment |> Vector.setMag(speed *. 1.0),
             separation |> Vector.setMag(speed *. 1.0),
             cohesion |> Vector.setMag(speed *. 1.0), */
          alignment,
          separation,
          cohesion,
        ])
        |> Vector.limitMag(maxForce) :
        Vector.create(0.0, 0.0);
    let body =
      birb.body
      |> Body.addForce(force)
      |> Body.update
      |> Body.limitVel(maxSpeed)
      |> Body.minVel(minSpeed)
      |> Body.wrap(env);
    {...birb, body};
  | None => birb
  };

let draw = (birb, ~hovered=false, ~selected=false, ~neighbor=false, env) => {
  Draw.pushMatrix(env);
  Draw.translate(~x=birb.body.pos.x, ~y=birb.body.pos.y, env);
  Draw.rotate(-. Vector.heading(birb.body.vel) +. Constants.pi, env);
  /* Draw.scale(~x=2.0, ~y=2.0, env); */
  /* Draw.pixelf(~pos=(0.0, 0.0), ~color=Constants.white, env); */
  if (selected) {
    Draw.fill(Constants.white, env);
  } else {
    Draw.noFill(env);
  };
  if (neighbor) {
    Draw.stroke(green, env);
  } else {
    Draw.stroke(white, env);
  };
  let length = 4.0;
  let halfLength = length /. 2.0;
  let width = 2.0;
  let halfWidth = width /. 2.0;
  Draw.trianglef(
    ~p1=(0.0, -. halfLength),
    ~p2=(halfWidth, halfLength),
    ~p3=(-. halfWidth, halfLength),
    env,
  );
  Draw.popMatrix(env);
};

let drawCircle_ = (env, birb) => {
  Draw.noFill(env);
  Draw.stroke(Constants.white, env);
  Draw.ellipsef(
    ~center=(birb.body.pos.x, birb.body.pos.y),
    ~radx=10.0,
    ~rady=10.0,
    env,
  );
};