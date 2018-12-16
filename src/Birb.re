open Reprocessing;

type t = {body: Body.t};

let create = body => {body: body};

let update = (env, birb) => {
  let body = birb.body |> Body.update |> Body.wrap(env);
  {...birb, body};
};

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

let drawBirb = (env, birb) => {
  Draw.pushMatrix(env);
  Draw.translate(~x=birb.body.pos.x, ~y=birb.body.pos.y, env);
  /* Draw.rotate(Vector.heading(birb.body.vel) -. Constants.half_pi, env); */
  Draw.rotate(-. Vector.heading(birb.body.vel) +. Constants.pi, env);
  /* Draw.pixelf(~pos=(0.0, 0.0), ~color=Constants.white, env); */
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