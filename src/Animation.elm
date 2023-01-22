module Animation exposing (..)


type alias Animation =
  { curve : Curve
  , duration : Float
  , time : Float
  , value : Float
  , start : Float
  , stop : Float
  , running : Bool
  , velocity : Float
  , initialVelocity : Float
  }


type alias Curve =
  Float -> Float -> Float -> Float -> Float


init : Curve -> Float -> Float -> Animation
init curve duration value =
  { curve = curve
  , duration = duration
  , time = 0
  , value = value
  , start = value
  , stop = value
  , running = False
  , velocity = 0
  , initialVelocity = 0
  }


withDuration : Float -> Animation -> Animation
withDuration duration animation =
  { animation | duration = duration }


to : Float -> Animation -> Animation
to stop animation =
  { animation
  | time = 0
  , start = animation.value
  , stop = stop
  , initialVelocity = animation.velocity
  }


run : Animation -> Animation
run animation =
  { animation
  | time = 0
  , running = True
  }


step : Float -> Animation -> Animation
step dt animation =
  if not animation.running || animation.start == animation.stop then
    animation

  else
    let
      t =
        animation.time + dt

      jump =
        animation.stop - animation.start

      value =
        animation.start + animation.curve
          animation.duration
          animation.initialVelocity
          jump
          t

      velocity =
        (value - animation.value) / dt

      velocityDelta =
        velocity - animation.velocity
    in
    if velocity * jump <= 0
      && velocityDelta * jump <= 0
    then
      { animation
      | time = t
      , value = animation.stop
      , velocity = 0
      , initialVelocity = 0
      }

    else
      { animation
      | time = t
      , value = value
      , velocity = velocity
      }


harmonic : Curve
harmonic dur vel jump t =
  let
    phi =
      2 * atan2 (vel * dur) (pi * jump)
  in
  jump * (cos phi - cos (phi + pi * t / dur)) / (cos phi + 1)


parabolic : Float -> Curve
parabolic a dur vel jump t =
  let
    d = 2 * jump - (1 + a) * vel * dur
  in
  if t < a * dur then
    vel * t + d / (2 * a) * (t / dur) ^ 2
  else
    jump - (d + vel * dur) / (2 * (1 - a)) * ((dur - t) / dur) ^ 2

