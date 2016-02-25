module Model (..) where

import Constants exposing (..) 

-- MODEL
type alias Ship =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , dir : Float
  , lastShot : Float
  , gun : List Bullet
  , size : ShipSize
  }
type alias ShipSize = {x: Int, y: Int}
type alias Bullet =
  {
    x : Float,
    y : Float,
    vx : Float,
    vy : Float,
    dir : Float,
    lifetime : Float
  }

type alias Keys = {x:Int, y:Int}
type alias Mouse = (Int, Int)
type alias MousePosition = (Float, Float)
type alias WindowDimensions = (Int, Int)
type alias ShootKey = Bool

newBullet : Ship -> Bullet
newBullet ship =
  let
    halfPi = pi / 2
    vx = bulletSpeed * cos(ship.dir + halfPi)
    vy = bulletSpeed * sin(ship.dir + halfPi)
  in
    {x = ship.x, y = ship.y, dir = ship.dir, vy = vy, vx = vx, lifetime = 0 }
