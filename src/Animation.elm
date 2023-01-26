module Animation exposing (..)


type alias Animation =
  { curve : Curve
  , period : Float
  , time : Float
  , value : Float
  , start : Float
  , stop : Float
  , running : Bool
  , velocity : Float
  , initialVelocity : Float
  }


init : Curve -> Float -> Float -> Animation
init curve period value =
  { curve = curve
  , period = period
  , time = 0
  , value = value
  , start = value
  , stop = value
  , running = False
  , velocity = 0
  , initialVelocity = 0
  }


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
  if not animation.running || done animation then
    animation

  else
    let
      t =
        animation.time + dt

      jump =
        animation.stop - animation.start

      value =
        animation.start + curveFunction animation.curve
          animation.period
          animation.initialVelocity
          jump
          t

      velocity =
        (value - animation.value) / dt

      velocityDelta =
        velocity - animation.velocity
    in
    if (value - animation.stop) * jump >= 0
      || velocity * jump <= 0
      && velocityDelta * jump <= 0
    then
      { animation
      | time = t
      , value = animation.stop
      , start = animation.stop
      , velocity = 0
      , initialVelocity = 0
      }

    else
      { animation
      | time = t
      , value = value
      , velocity = velocity
      }


done : Animation -> Bool
done animation =
  animation.start == animation.stop


type Curve
  = Harmonic
  | Parabolic Float
  | Custom CurveFunction


curveFunction : Curve -> CurveFunction
curveFunction curve =
  case curve of
    Harmonic -> harmonic
    Parabolic a -> parabolic a
    Custom function -> function


type alias CurveFunction =
  Float -> Float -> Float -> Float -> Float


harmonic : CurveFunction
harmonic dur vel jump t =
  let
    phi =
      2 * atan2 (vel * dur) (pi * jump)
  in
  jump * (cos phi - cos (phi + pi * t / dur)) / (cos phi + 1)


parabolic : Float -> CurveFunction
parabolic a dur vel jump t =
  let
    d = 2 * jump - (1 + a) * vel * dur
  in
  if t < a * dur then
    vel * t + d / (2 * a) * (t / dur) ^ 2
  else
    jump - (d + vel * dur) / (2 * (1 - a)) * (1 - t / dur) ^ 2

