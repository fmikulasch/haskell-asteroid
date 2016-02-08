module Settings where

import Graphics.Gloss.Data.Color

-- Window settings
sWidth, sHeight :: Float
sWidth  = 700
sHeight = 500

sBGColor, sStarColor :: Color
sBGColor   = makeColor 0.05 0.05 0.08 1.0
sStarColor = makeColor 0.9 0.8 0.9 0.5

sUpdate :: Int
sUpdate = 100

sRandGen :: Int
sRandGen = 1

sStepSize :: Float
sStepSize = 1 / fromIntegral sUpdate

sRespawnTime :: Float
sRespawnTime = 7

-- Ship settings
sTurnSpeed, sMaxSpeed, sAcceleration, sReloadTime, sShipSize :: Float
sTurnSpeed = 2.5
sAcceleration = 2
sMaxSpeed = 120
sReloadTime = 0.2
sShipSize = 11

-- Bullet settings
sBulletSize, sBulletSpeed, sBulletLength, sBulletLife :: Float
sBulletSpeed  = 400
sBulletSize   = 5
sBulletLength = 3
sBulletLife   = 1

-- Asteroid settings
sAsteroidSize, sMinAsteroidSize, sAsteroidSpeed, sAsteroidSpeedIncrease, sAsteroidRotation :: Float
sAsteroidSize     = 50
sMinAsteroidSize  = 16
sAsteroidSpeed    = 160
sAsteroidSpeedIncrease = 1.4
sAsteroidRotation = 0.6

sMaxAsteroids :: Float
sMaxAsteroids = 4

sExplosionTime :: Float
sExplosionTime = 2.5

sPulseTime, sPulseSpeed, sPulseDistance :: Float
sPulseTime = 0.26
sPulseSpeed = -0.5
sPulseDistance = 15
