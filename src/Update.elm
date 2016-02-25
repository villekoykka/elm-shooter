module Update (..) where
import Debug
import Model exposing (..)
import Constants exposing (..)

-- UPDATE
update : (WindowDimensions, Float, Keys, ShootKey, Mouse) -> Ship -> Ship
update (windowDim, dt, keys, actions, mouse) ship =
  let 
    mousePos = mousePosition windowDim mouse
    log = Debug.watch "Ship dir" ship.dir
  in
    ship
      |> moveShip dt keys
      |> turnShip dt mousePos
      |> shoot dt actions
      |> moveBullets dt

mousePosition : WindowDimensions -> Mouse -> MousePosition
mousePosition windowDim mousePos =
  let
    (wX, wY) = windowDim
    (mX, mY) = mousePos
    x = toFloat mX - (toFloat wX/2)
    y = toFloat mY - (toFloat wY/2)
  in
    (x, y * -1)

--  GUN / SHOOTING
shoot : Float -> ShootKey -> Ship -> Ship
shoot dt actions ship =
  let
    canShoot = gunCoolDown dt ship
    shoot = if canShoot && actions then True else False

    bullets = case shoot of
      True  -> List.append [newBullet ship] ship.gun
      False -> ship.gun

    lastShot = case shoot of
      True -> 0
      False -> ship.lastShot + dt
  in
    {ship | gun = bullets, lastShot = lastShot}

gunCoolDown : Float -> Ship -> Bool
gunCoolDown dt ship =
  if ship.lastShot > gunCoolDownDelay then True else False

-- BULLETS
moveBullets : Float -> Ship -> Ship
moveBullets dt ship =
  let
    bulletFilter lt bullet = if bullet.lifetime > lt then False  else True
    bulletsOnTheAir = List.filter (bulletFilter bulletLifetime) ship.gun
    bullets = List.map (moveBullet dt) bulletsOnTheAir
  in
    {ship | gun = bullets }

moveBullet : Float -> Bullet -> Bullet
moveBullet dt bullet =
  let
    x = bullet.x + (bullet.vx * dt)
    y = bullet.y + (bullet.vy * dt)
    lifetime = bullet.lifetime + dt
  in
    {bullet | x = x, y = y, lifetime = lifetime}

-- SHIP MOVEMENT
turnShip : Float -> MousePosition -> Ship -> Ship
turnShip dt mouse ship =
  let
    (mx, my) = mouse
    xl = mx - ship.x
    yl = my - ship.y
    angle =  atan2 yl xl
  in
    {ship | dir = angle - pi / 2}

moveShip : Float -> Keys -> Ship -> Ship
moveShip dt keys ship =
  let
    speed = dt * shipSpeed
    x = ship.x + (toFloat(keys.x) * speed)
    y = ship.y + (toFloat(keys.y) * speed)
  in
    {ship | x = x, y = y}
