type t = {
  pos: Vector.t,
  vel: Vector.t,
  acc: Vector.t,
};

let create =
    (
      ~pos=Vector.create(0.0, 0.0),
      ~vel=Vector.create(0.0, 0.0),
      ~acc=Vector.create(0.0, 0.0),
      (),
    )
    : t => {
  pos,
  vel,
  acc,
};

let update = body => {
  let pos = Vector.add(body.pos, body.vel);
  let vel = Vector.add(body.vel, body.acc);
  {pos, vel, acc: Vector.create(0.0, 0.0)};
};

let addForce = (force, body) => {...body, acc: Vector.add(body.acc, force)};

let wrap = (env, body) => {...body, pos: Vector.wrap(env, body.pos)};

let limitVel = (limit, body) => {
  ...body,
  vel: Vector.limitMag(limit, body.vel),
};