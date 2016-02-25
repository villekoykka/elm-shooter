module View (..) where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

import Debug
import Model exposing(..)

-- VIEW
view : (Int, Int) -> Ship -> Element
view (w',h') ship =
    let
      bullets = List.map drawBullet ship.gun
      w = toFloat w'
      h = toFloat h'
    in
      collage w' h'
      (List.append
        [ rect w h
              |> filled (rgb 174 238 238),
          drawShip ship
       ] bullets)

drawShip : Ship -> Form
drawShip ship =
  let
     position = (ship.x, ship.y)
     imgSrc = "ship.png"
     size = ship.size
     img = image size.x size.y imgSrc
  in
    img
      |> toForm
      |> move position
      |> rotate ship.dir

drawBullet : Bullet -> Form
drawBullet bullet =
  rect 8 8
    |> filled (rgb 232 34 111)
    |> rotate bullet.dir
    |> move (bullet.x, bullet.y)

