module Game (stepState, handleInput) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import System.Random
import Debug.Trace
import Data.Fixed
import State
import Settings

----- Updating Functions -----

stepState :: Float -> State -> State
stepState t (State ship asteroids bullets keys gen)
    = checkCollisions
    $ applyInput
    $ State (moveShip t ship)
            (updateAsteroids t asteroids r s)
            (updateBullets t bullets)
            keys
            gen''

    where (r,gen')  = random gen  :: (Float, StdGen)
          (s,gen'') = random gen' :: (Float, StdGen)


--- Moving

moveShip :: Float -> Ship -> Ship
moveShip t (Ship vel pos alpha reload 0.0)
    = Ship vel
           (updatePosition t pos vel 20)
           alpha
           (reload - t)
           0.0

moveShip t (Ship _ pos _ _ dead)
   = Ship (0.0,0.0)
          pos
          0.0
          1.0
          (dead + t)

updateAsteroids :: Float -> [Asteroid] -> Float -> Float -> [Asteroid]
updateAsteroids t asteroids r s
    = filter ((>) sExplosionTime . asteroidExploding)
    $ (if sMaxAsteroids * r > fromIntegral (length asteroids)
         then [newAsteroid]
         else [])
    ++ map (moveAsteroid t) asteroids

    where newAsteroid =
            Asteroid (r * sAsteroidSpeed,s * sAsteroidSpeed)
                     (s * sWidth,r * sHeight)
                     0.0
                     (r * sAsteroidRotation)
                     (r * sAsteroidSize)
                     0.0

moveAsteroid :: Float -> Asteroid -> Asteroid
moveAsteroid t (Asteroid vel pos alpha dalpha size 0.0)
    = Asteroid vel
               (updatePosition t pos vel size)
               (alpha + dalpha `mod'` 360)
               dalpha
               size
               0.0

moveAsteroid t (Asteroid _ pos _ _ _ dead)
   = Asteroid (0,0)
              pos
              0.0
              0.0
              0.0
              (dead + t)

updateBullets :: Float -> [Bullet] -> [Bullet]
updateBullets t = filter ((> 0) . bulletLife) . map (updateBullet t)

updateBullet :: Float -> Bullet -> Bullet
updateBullet t (Bullet vel pos life)
    = Bullet vel
             (updatePosition t pos vel sBulletSize)
             (life - t)

updatePosition :: Float -> Point -> Vector -> Float -> Point
updatePosition t pos vel size
    = moveToScreen (pos + t `mulSV` vel) size

moveToScreen :: Point -> Float -> Point
moveToScreen (x,y) s
    | y - s >   sHeight / 2 = (x, - sHeight / 2 - s)
    | y + s < - sHeight / 2 = (x,   sHeight / 2 + s)
    | x - s >   sWidth / 2  = (- sWidth / 2 - s, y)
    | x + s < - sWidth / 2  = (  sWidth / 2 + s, y)
    | otherwise  = (x,y)


--- Applying Input

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
handleShip keys (Ship vel pos alpha reload 0.0)
    = Ship vel' pos alpha'' reload' 0.0

    where vel' =
            if (SpecialKey KeyUp) `elem` keys
                then updateSpeed vel alpha
                else vel

          alpha' =
            if (SpecialKey KeyLeft) `elem` keys
                then (alpha + sTurnSpeed) `mod'` 360
                else alpha

          alpha'' =
            if (SpecialKey KeyRight) `elem` keys
                then (alpha - sTurnSpeed) `mod'` 360
                else alpha'

          reload' =
            if (SpecialKey KeySpace) `elem` keys
                && reload < 0
                then sReloadTime
                else reload

handleShip _ ship = ship

updateSpeed :: Vector -> Float -> Vector
updateSpeed vel alpha
    | magV vel' > sMaxSpeed = vel
    | otherwise             = vel'
    where vel' = vel + (sAcceleration `mulSV` toVector alpha)

createBullet :: Ship -> [Bullet]
createBullet (Ship vel pos alpha reload _)
    | reload < 0
        = let vel' = (vel + sBulletSpeed `mulSV` toVector alpha) in
          [Bullet vel'
                  (pos + (20 / sBulletSpeed) `mulSV` vel')
                   sBulletLife]
    | otherwise  = []


--- Checking for Collisions

checkCollisions :: State -> State
checkCollisions (State ship asteroids bullets keys gen)
    = State ship' asteroids'' bullets' keys gen
    where (ship',asteroids')     = foldl collideShipAsteroids (ship,[]) asteroids
          (bullets',asteroids'') = foldl collideBulletsAsteroids (bullets,[]) asteroids'

collideShipAsteroids :: (Ship,[Asteroid]) -> Asteroid -> (Ship,[Asteroid])
collideShipAsteroids (ship@(Ship vel pos alpha reload 0.0),asteroids) asteroid
    | collides (shipPosition ship) sShipSize
               (asteroidPosition asteroid) (asteroidSize asteroid)
    && asteroidExploding asteroid == 0
           = ((Ship vel pos alpha reload sStepSize), explodeAsteroid asteroid ++ asteroids)
    | otherwise = (ship,asteroid : asteroids)

collideShipAsteroids (ship,asteroids) asteroid
    = (ship,asteroid:asteroids)

collideBulletsAsteroids :: ([Bullet],[Asteroid]) -> Asteroid -> ([Bullet],[Asteroid])
collideBulletsAsteroids (bullets,asteroids) asteroid
    = (bullets',asteroid' ++ asteroids)
    where (asteroid',bullets') = foldl collideAsteroidBullets ([asteroid],[]) bullets

collideAsteroidBullets :: ([Asteroid],[Bullet]) -> Bullet -> ([Asteroid],[Bullet])
collideAsteroidBullets ((asteroid:[]),bullets) bullet
    | collides (asteroidPosition asteroid) (asteroidSize asteroid)
               (bulletPosition bullet) sBulletSize
    && asteroidExploding asteroid == 0
           = (explodeAsteroid asteroid,bullets)
    | otherwise = (asteroid:[],bullet:bullets)

collideAsteroidBullets (asteroids,bullets) bullet
    = (asteroids,bullet:bullets)

collides :: Point -> Float -> Point -> Float -> Bool
collides p1 s1 p2 s2
    = magV (p1 - p2) < s1 + s2

explodeAsteroid :: Asteroid -> [Asteroid]
explodeAsteroid (Asteroid vel pos alpha dalpha size _)
    = Asteroid (0.0,0.0) pos 0.0 0.0 size sStepSize
    : newAsteroids
    where newAsteroids  = if size > sMinAsteroidSize
                            then [newAsteroid (-), newAsteroid (+)]
                            else []
          newAsteroid f = Asteroid ((f 0 1.5, 1.5) * vel)
                                   pos
                                   alpha
                                   (f 0 dalpha)
                                   (size / 2)
                                   0.0


----- Input Function -----

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

toVector :: Float -> Vector
toVector = unitVectorAtAngle . degToRad
