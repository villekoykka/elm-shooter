import Keyboard
import Time exposing (..)
import Window
import Mouse
import Graphics.Element exposing(Element)

import Model exposing (..)
import Update exposing (..)
import View exposing (..)

-- DEFAULT STATE
ship : Ship
ship =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , dir = 0
  , lastShot = 0
  , gun = []
  , size = {x = 45, y = 45}
  }

-- SIGNALS
main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update (Debug.watch "Ship" ship) input)

input : Signal ((Int, Int), Float, Keys, ShootKey, Mouse)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)
  in
    Signal.sampleOn delta (
      Signal.map5 (,,,,)
        Window.dimensions
        delta
        Keyboard.wasd
        Mouse.isDown
        Mouse.position
    )
