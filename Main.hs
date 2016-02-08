import Control.Lens
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import State
import Game
import Settings
import Data.Fixed
import Debug.Trace

main
 = play (InWindow "Asteroid" (floor sWidth, floor sHeight) (100, 100))
        sBGColor
        sUpdate
        menuState
        drawState
        handleInput
        stepState


----- Rendering Functions -----

drawState :: State -> Picture
drawState state
    | state^.stateType == Menu
    = Pictures $
    [ Color white $ Translate (-37) 10 $ Scale 0.3 0.3 $ Text "Play"
    , Color white $ Translate (-50) (-15) $ Scale 0.1 0.1 $ Text "Press any key"
    ] ++ stars

    | otherwise
    = Pictures
    $ drawShip (state^.stateShip)
    : stars
    ++ map drawBullet (state^.stateBullets)
    ++ map drawEffect (state^.stateEffects)
    ++ map drawAsteroid (state^.stateAsteroids)


drawAsteroid :: Asteroid -> Picture
drawAsteroid asteroid
    | asteroid^.asteroidExploding == 0.0
    = Color (dark white)
    $ Translate x y
    $ Rotate (asteroid^.asteroidRotation)
    $ asteroidPicture (asteroid^.asteroidShape) (asteroid^.asteroidSize)

    | otherwise
    = drawExplosion (asteroid^.asteroidPosition) (asteroid^.asteroidExploding)

    where (x,y) = asteroid^.asteroidPosition

drawBullet :: Bullet -> Picture
drawBullet bullet
    = Color white
    $ Translate x y
    $ Line [(0,0),sBulletLength `mulSV` normalizeV (bullet^.bulletVelocity)]

    where (x,y) = bullet^.bulletPosition

drawShip :: Ship -> Picture
drawShip ship
    | dead == 0.0
    = Color white
    $ Translate x y
    $ Rotate (-alpha) shipPicture

    | otherwise
    = Color white
    $ Translate x y
    $ Rotate (-alpha)
    $ wreckPicture dead

    where (x,y) = ship^.shipPosition
          alpha = ship^.shipRotation
          dead  = ship^.shipExploding

drawExplosion :: Point -> Float -> Picture
drawExplosion (x,y) t
    = Color (makeColor 1.0 1.0 1.0 (1.0 - t / sExplosionTime))
    $ Translate x y
    $ Scale t t
    $ Pictures
    [ explosionPicture x y
    , Scale t t
    $ Rotate 1.5 (explosionPicture x y)
    ]

drawEffect :: Effect -> Picture
drawEffect (Pulse pos@(x,y) alpha time)
    = Color (makeColor 1.0 1.0 1.0 (1.0 - time / sPulseTime))
    $ Translate x' y'
    $ Scale time time
    $ Line [pos', pos' + 4 `mulSV` direction]
    where direction = toVector alpha
          (x',y') = pos - sPulseDistance `mulSV` direction
          pos' = (10 * (x `mod'` 2), 10 * (y `mod'` 3))

shipPicture :: Picture
shipPicture
    = Line
    [ (20,0)
    , (-10,10)
    , (-4,0)
    , (-10,-10)
    , (20,0)
    ]

wreckPicture :: Float -> Picture
wreckPicture t
    = Pictures
    [ Translate 0 (1*t)
    $ Rotate (1.5 * t)
    $ Line [(20,0), (5,5)]
    , Translate 0 (-4*t)
    $ Rotate (3 * t)
    $ Line [(5,5), (-10,10)]
    , Translate (0.5*t) (0.5*t)
    $ Rotate (0 * t)
    $ Line [(-10,10), (-4,0)]
    , Translate 0 (-2*t)
    $ Rotate (-3 * t)
    $ Line [(-4,0), (-10,-10)]
    , Translate (1.5*t) 0
    $ Line [(-10,-10), (5,-5)]
    , Translate (-2.2*t) 0
    $ Rotate (-8 * t)
    $ Line [(5,-5), (20,0)]
    ]

asteroidPicture :: Int -> Float -> Picture
asteroidPicture 0 x
    = Scale (x * 1/15) (x * 1/15)
    $ Line
    [ (11.0,7.0)
    , (-2.0,12.0)
    , (-11.0,9.0)
    , (-14.0,-1.0)
    , (-11.0,-13.0)
    , (0.0,-10.0)
    , (9.0,-12.0)
    , (14.0,-3.0)
    , (7.0,2.0)
    , (11.0,7.0)
    ]

asteroidPicture 1 x
    = Scale (x * 1/15) (x * 1/15)
    $ Line
    [ (11.0,7.0)
    , (-4.0,10.0)
    , (-10.0,9.0)
    , (-18.0,2.0)
    , (-9.0,-10.0)
    , (0.0,-12.0)
    , (7.0,-10.0)
    , (14.0,-3.0)
    , (8.0,3.0)
    , (11.0,7.0)
    ]

asteroidPicture _ x
    = Scale (x * 1/15) (x * 1/15)
    $ Line
    [ (11.0,7.0)
    , (0.0,10.0)
    , (-4.0,6.0)
    , (-12.0,2.0)
    , (-15.0,-5.0)
    , (-1.0,-12.0)
    , (7.0,-10.0)
    , (14.0,-3.0)
    , (8.0,3.0)
    , (11.0,7.0)
    ]

explosionPicture :: Float -> Float -> Picture
explosionPicture x y
    = let x' = x `mod'` 3
          y' = y `mod'` 3 in
    Pictures
    [ Line [from,normalizeV from + from]
    | from <- [ a * b
              | a <- [(1,3 * y'),(4 * x',1),(2 * x',8),(5,5)]
              , b <- [(-1,1),(y',x'),(- y',- 0.4),(1,- y')]
              ]
    ]

stars :: [Picture]
stars =
      [ Color sStarColor
      $ Translate (x / 360 * sWidth - sWidth / 2)
                  (y / 360 * sHeight - sHeight / 2)
      $ Line [(0,0), (1,0)]
      | (x,y) <- [ (227,351), (261,335), (315,359)
                 , (15,69), (67,19), (162,4.6)
                 , (209,335), (213,13), (290,302)
                 , (199,351), (311,1), (47,350)
                 , (300,3), (181,340), (351,15)
                 , (316,50), (192,23), (20,295)
                 , (30,50), (4,29), (60,5)
                 , (122,61), (100,65), (240,190)
                 , (145,120), (87,304), (92,61)
                 , (89,170), (239,260), (120,122)
                 , (178,188), (190,201), (170,160)
                 ]
      ]
