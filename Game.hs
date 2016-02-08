module Game (stepState, handleInput, toVector) where

import Control.Lens
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import System.Random
import Data.Fixed
import State
import Settings

----- Updating Functions -----

stepState :: Float -> State -> State
stepState t state
    | state^.stateType == Menu
    = state

    | state^.stateShip.shipExploding > sRespawnTime
    = set stateType Menu state

    | otherwise
    = checkCollisions
    $ applyInput
    $ set randGen gen''
    $ over stateShip (moveShip t)
    $ over stateAsteroids (\as -> updateAsteroids t as r s)
    $ over stateBullets (updateBullets t)
    $ over stateEffects (updateEffects t)
    $ state

    where gen       = state^.randGen
          (r,gen')  = random gen  :: (Float, StdGen)
          (s,gen'') = random gen' :: (Float, StdGen)


--- Moving

moveShip :: Float -> Ship -> Ship
moveShip t s
    | dead == 0
    = over shipPosition (\pos -> updatePosition t pos vel sShipSize)
    $ over shipReload   (\r -> r - t)
    $ s

    | otherwise
    = over shipPosition  (\pos -> updatePosition t pos vel sShipSize)
    $ over shipRotation  (+ s^.shipReload)
    $ over shipExploding (+ t)
    $ s
    where vel = s^.shipVelocity
          dead = s^.shipExploding

updateAsteroids :: Float -> [Asteroid] -> Float -> Float -> [Asteroid]
updateAsteroids t asteroids r s
    = filter ((>) sExplosionTime . _asteroidExploding)
    $ (if sMaxAsteroids * r > fromIntegral (length asteroids)
         then [newAsteroid]
         else [])
    ++ map (moveAsteroid t) asteroids

    where newAsteroid =
            Asteroid ((r - 0.5) * sAsteroidSpeed,(s - 0.5) * sAsteroidSpeed)
                     pos
                     0.0
                     (r * sAsteroidRotation)
                     size
                     0.0
                     (round (s * 3 - 0.5))
          pos = if s < 0.5
                    then ((s - 0.5) * sWidth, sHeight / 2 + size)
                    else (sWidth / 2 + size, (r - 0.5) * sHeight)
          size = s * sAsteroidSize + sMinAsteroidSize

moveAsteroid :: Float -> Asteroid -> Asteroid
moveAsteroid t asteroid
    | asteroid^.asteroidExploding == 0.0
    = over asteroidPosition (\ pos -> updatePosition t pos vel size)
    $ over asteroidRotation (+ mom `mod'` 360)
    $ asteroid

    | otherwise
    = over asteroidExploding (+ t)
    $ asteroid

    where vel  = asteroid^.asteroidVelocity
          size = asteroid^.asteroidSize
          mom  = asteroid^.asteroidMomentum

updateBullets :: Float -> [Bullet] -> [Bullet]
updateBullets t = filter ((> 0) . _bulletLife) . map (updateBullet t)

updateBullet :: Float -> Bullet -> Bullet
updateBullet t bullet
    = over bulletPosition (\ pos -> updatePosition t pos vel sBulletSize)
    $ over bulletLife (\l -> l - t)
    $ bullet

    where vel = bullet^.bulletVelocity

updateEffects :: Float -> [Effect] -> [Effect]
updateEffects t = filter ((> 0) . _pulseTime) . map (updateEffect t)

updateEffect :: Float -> Effect -> Effect
updateEffect t pulse
    = over pulsePosition (\pos -> pos + sPulseSpeed `mulSV` toVector alpha)
    $ over pulseTime (\time -> time - t)
    $ pulse

    where alpha = pulse^.pulseAlpha

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
applyInput state
    = over stateShip (handleShip keys)
    $ over stateBullets (++ newBullet)
    $ over stateEffects (++ newEffects)
    $ state

    where keys = state^.stateKeys

          newBullet =
            if (SpecialKey KeySpace) `elem` keys
                then createBullet $ state^.stateShip
                else []

          newEffects =
            if (SpecialKey KeyUp) `elem` keys
                then createPulse $ state^.stateShip
                else []

handleShip :: [Key] -> Ship -> Ship
handleShip keys ship
    | ship^.shipExploding == 0.0
    = over shipVelocity uVel
    $ over shipReload uReload
    $ over shipRotation (uAlpha . uAlpha')
    $ ship

    | otherwise
    = ship

    where uVel vel =
            if (SpecialKey KeyUp) `elem` keys
                then updateSpeed vel (ship^.shipRotation)
                else vel

          uAlpha alpha =
            if (SpecialKey KeyLeft) `elem` keys
                then (alpha + sTurnSpeed) `mod'` 360
                else alpha

          uAlpha' alpha =
            if (SpecialKey KeyRight) `elem` keys
                then (alpha - sTurnSpeed) `mod'` 360
                else alpha

          uReload reload =
            if (SpecialKey KeySpace) `elem` keys
                && reload < 0
                then sReloadTime
                else reload

updateSpeed :: Vector -> Float -> Vector
updateSpeed vel alpha
    | magV vel' > sMaxSpeed = (sMaxSpeed / magV vel') `mulSV` vel'
    | otherwise             = vel'
    where vel' = vel + (sAcceleration `mulSV` toVector alpha)

createBullet :: Ship -> [Bullet]
createBullet ship
    | reload < 0 && dead == 0
        = [ Bullet vel'
                   (pos + (20 / sBulletSpeed) `mulSV` vel')
                   sBulletLife
          ]
    | otherwise  = []

    where reload = ship^.shipReload
          dead   = ship^.shipExploding
          pos    = ship^.shipPosition
          vel    = ship^.shipVelocity
          vel'   = (vel + sBulletSpeed `mulSV` toVector alpha)
          alpha  = ship^.shipRotation

createPulse :: Ship -> [Effect]
createPulse ship
    | ship^.shipExploding == 0
        = [ Pulse (pos- 5 `mulSV` toVector (ship^.shipRotation))
                  alpha
                  sPulseTime
          ]
    | otherwise = []

    where alpha = ship^.shipRotation
          pos   = ship^.shipPosition


--- Checking for Collisions

checkCollisions :: State -> State
checkCollisions state
    = set stateShip ship'
    $ set stateBullets bullets'
    $ set stateAsteroids asteroids''
    $ state

    where asteroids              = state^.stateAsteroids
          ship                   = state^.stateShip
          bullets                = state^.stateBullets
          (ship',asteroids')     = foldl collideShipAsteroids (ship,[]) asteroids
          (bullets',asteroids'') = foldl collideBulletsAsteroids (bullets,[]) asteroids'

collideShipAsteroids :: (Ship,[Asteroid]) -> Asteroid -> (Ship,[Asteroid])
collideShipAsteroids (ship@(Ship velS posS alphaS reloadS 0.0),asteroids)
                      asteroid@(Asteroid velA posA alphaA dalphaA sizeA 0.0 _)
    | collides posS sShipSize posA sizeA
           = (Ship ((25 / (sizeA + 20)) `mulSV` velS + (sizeA / 60) `mulSV` velA )
                    posS
                    alphaS
                    (- dalphaA)
                    sStepSize
             , explodeAsteroid asteroid ++ asteroids)
    | otherwise = (ship,asteroid : asteroids)

collideShipAsteroids (ship,asteroids) asteroid
    = (ship,asteroid:asteroids)

collideBulletsAsteroids :: ([Bullet],[Asteroid]) -> Asteroid -> ([Bullet],[Asteroid])
collideBulletsAsteroids (bullets,asteroids) asteroid
    = (bullets',asteroid' ++ asteroids)
    where (asteroid',bullets') = foldl collideAsteroidBullets ([asteroid],[]) bullets

collideAsteroidBullets :: ([Asteroid],[Bullet]) -> Bullet -> ([Asteroid],[Bullet])
collideAsteroidBullets ((asteroid:[]),bullets) bullet
    | collides (asteroid^.asteroidPosition) (asteroid^.asteroidSize)
               (bullet^.bulletPosition) sBulletSize
      && asteroid^.asteroidExploding == 0
           = (explodeAsteroid asteroid,bullets)
    | otherwise = (asteroid:[],bullet:bullets)

collideAsteroidBullets (asteroids,bullets) bullet
    = (asteroids,bullet:bullets)

collides :: Point -> Float -> Point -> Float -> Bool
collides p1 s1 p2 s2
    = magV (p1 - p2) < s1 + s2

explodeAsteroid :: Asteroid -> [Asteroid]
explodeAsteroid (Asteroid vel pos alpha dalpha size _ shape)
    = Asteroid (0.0,0.0) pos 0.0 0.0 size sStepSize shape
    : newAsteroids

    where newAsteroids  = if size > sMinAsteroidSize
                            then [newAsteroid (-0.5) , newAsteroid 0.5]
                            else []
          newAsteroid d = Asteroid (sAsteroidSpeedIncrease `mulSV` (rotateV d vel))
                                   pos
                                   alpha
                                   dalpha
                                   (size / 2)
                                   0.0
                                   shape


----- Input Function -----

handleInput :: Event -> State -> State
handleInput (EventKey key keystate _ _) state
    | state^.stateType == Menu && keystate == Down
    = initialState

    | otherwise
    = over stateKeys uKeys state

    where uKeys keys =
            case keystate of
                Up   -> filter ((/=) key) keys
                Down -> key : keys

handleInput _ state = state

toVector :: Float -> Vector
toVector = unitVectorAtAngle . degToRad
