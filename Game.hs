module Game where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import State
import System.Random
import Debug.Trace
import Data.Fixed

----- Updating Functions -----


width, height :: Float
width  = 700
height = 500

stepState :: Float -> State -> State
stepState t (State ship asteroids bullets keys gen)
    = applyInput
    $ State (moveShip t ship)
            (updateAsteroids t asteroids r s)
            (updateBullets t bullets)
            keys
            gen''

    where (r,gen')  = random gen  :: (Float, StdGen)
          (s,gen'') = random gen' :: (Float, StdGen)

moveShip :: Float -> Ship -> Ship
moveShip t (Ship vel pos alpha damage reload)
    = Ship vel
           (updatePosition t pos vel 20)
           alpha
           damage
           (reload - t)

updateAsteroids :: Float -> [Asteroid] -> Float -> Float -> [Asteroid]
updateAsteroids t asteroids r s
    = (if floor (4 * r) > length asteroids
         then [newAsteroid]
         else [])
    ++ map (moveAsteroid t) asteroids

    where newAsteroid =
            Asteroid (r * 100,s * 100)
                     (s * width,r * height)
                     (r * 100)

moveAsteroid :: Float -> Asteroid -> Asteroid
moveAsteroid t (Asteroid vel pos size)
    = Asteroid vel
               (updatePosition t pos vel size)
               size

updateBullets :: Float -> [Bullet] -> [Bullet]
updateBullets t = filter ((> 0) . bulletLife) . map (updateBullet t)

updateBullet :: Float -> Bullet -> Bullet
updateBullet t (Bullet vel pos life)
    = Bullet vel
             (updatePosition t pos vel 5)
             (life - t)

updatePosition :: Float -> Point -> Vector -> Float -> Point
updatePosition t pos vel size
    = moveToScreen (pos + t `mulSV` vel) size

moveToScreen :: Point -> Float -> Point
moveToScreen (x,y) s
    | y - s >   height / 2 = (x, - height / 2 - s)
    | y + s < - height / 2 = (x,   height / 2 + s)
    | x - s >   width / 2  = (- width / 2 - s, y)
    | x + s < - width / 2  = (  width / 2 + s, y)
    | otherwise  = (x,y)


applyInput :: State -> State
applyInput (State ship asteroids bullets keys gen)
    = State (handleShip keys ship)
            asteroids
            (newBullet ++ bullets)
            keys
            gen

    where newBullet =
            if (SpecialKey KeySpace) `elem` keys
                then createBullet ship
                else []

handleShip :: [Key] -> Ship -> Ship
handleShip keys (Ship vel pos alpha damage reload)
    = Ship vel' pos alpha'' damage reload'

    where vel' =
            if (SpecialKey KeyUp) `elem` keys
                then updateSpeed vel alpha
                else vel

          alpha' =
            if (SpecialKey KeyLeft) `elem` keys
                then (alpha + 2) `mod'` 360
                else alpha

          alpha'' =
            if (SpecialKey KeyRight) `elem` keys
                then (alpha - 2) `mod'` 360
                else alpha'

          reload' =
            if (SpecialKey KeySpace) `elem` keys
                && reload < 0
                then 0.5
                else reload

updateSpeed :: Vector -> Float -> Vector
updateSpeed vel alpha
    | magV vel' > 200 = vel
    | otherwise       = vel'
    where vel' = vel + (3 `mulSV` (unitVectorAtAngle . degToRad) alpha)

createBullet :: Ship -> [Bullet]
createBullet (Ship _ pos alpha _ reload)
    | reload < 0 = [Bullet (100 `mulSV` (unitVectorAtAngle . degToRad) alpha)
                           pos
                           2]
    | otherwise  = []


----- Input Functions -----

handleInput :: Event -> State -> State
handleInput (EventKey key keystate _ _)
            (State ship asteroids bullets keys gen)
    = State ship
            asteroids
            bullets
            keys'
            gen

    where keys' =
            case keystate of
                Up   -> filter ((/=) key) keys
                Down -> key : keys

handleInput _ state = state