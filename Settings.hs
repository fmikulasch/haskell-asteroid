module Settings where

import Graphics.Gloss.Data.Color

-- Window settings
sWidth, sHeight :: Float
sWidth  = 700
sHeight = 500

sBGColor :: Color
sBGColor = makeColor 0.05 0.05 0.08 1.0

sUpdate :: Int
sUpdate = 100

sStepSize :: Float
sStepSize = 1 / fromIntegral sUpdate

-- Ship settings
sTurnSpeed, sMaxSpeed, sAcceleration, sReloadTime, sShipSize :: Float
sTurnSpeed = 2
sAcceleration = 3
sMaxSpeed = 200
sReloadTime = 0.2
sShipSize = 20

-- Bullet settings
sBulletSize, sBulletSpeed, sBulletLength, sBulletLife :: Float
sBulletSpeed  = 400
sBulletSize   = 5
sBulletLength = 3
sBulletLife   = 1

-- Asteroid settings
sAsteroidSize, sMinAsteroidSize, sAsteroidSpeed, sAsteroidRotation :: Float
sAsteroidSize     = 100
sMinAsteroidSize  = 25
sAsteroidSpeed    = 100
sAsteroidRotation = 3

sMaxAsteroids :: Float
sMaxAsteroids = 4

sExplosionTime :: Float
sExplosionTime = 2.5
