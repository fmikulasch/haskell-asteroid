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
        Menu
        drawState
        handleInput
        stepState


----- Rendering Functions -----

drawState :: State -> Picture
drawState (State ship asteroids bullets effects keys _)
    = Pictures
    $ drawShip ship
    : stars
    ++ map drawBullet bullets
    ++ map drawEffect effects
    ++ map drawAsteroid asteroids

drawState Menu
    = Pictures $
    [ Color white $ Translate (-37) 10 $ Scale 0.3 0.3 $ Text "Play"
    , Color white $ Translate (-50) (-15) $ Scale 0.1 0.1 $ Text "Press any key"
    ] ++ stars

drawAsteroid :: Asteroid -> Picture
drawAsteroid (Asteroid _ (x,y) alpha _ size 0.0)
    = Color (dark white)
    $ Translate x y
    $ Rotate alpha
    $ asteroidPicture size

drawAsteroid (Asteroid _ pos _ _ _ dead)
    = drawExplosion pos dead

drawBullet :: Bullet -> Picture
drawBullet (Bullet vel pos@(x,y) _)
    = Color white
    $ Translate x y
    $ Line [(0,0),sBulletLength `mulSV` normalizeV vel]

drawShip :: Ship -> Picture
drawShip (Ship _ (x,y) alpha _ 0.0)
    = Color white
    $ Translate x y
    $ Rotate (-alpha) shipPicture

drawShip (Ship _ pos _ _ dead)
    = drawExplosion pos dead

drawExplosion :: Point -> Float -> Picture
drawExplosion (x,y) t
    = Color (makeColor 1.0 1.0 1.0 (1.0 - t / sExplosionTime))
    $ Translate x y
    $ Scale t t
    $ Pictures
    [ explosionPicture x y
    , Scale t t
    $ Rotate 1.2 (explosionPicture x y)
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

asteroidPicture :: Float -> Picture
asteroidPicture x
    = Scale (x * 1/15) (x * 1/15)
    $ Line
    [ (11.0,7.0)
    , (-2.0,12.0)
    , (-11.0,9.0)
    , (-17.0,-1.0)
    , (-11.0,-13.0)
    , (0.0,-10.0)
    , (9.0,-12.0)
    , (14.0,-3.0)
    , (7.0,2.0)
    , (11.0,7.0)
    ]

explosionPicture :: Float -> Float -> Picture
explosionPicture x y
    = let x' = x `mod'` 2
          y' = y `mod'` 2 in
    Pictures
    [ Line [from,normalizeV from + from]
    | from <- [ a * b
              | a <- [(1,3),(4,1),(2,8),(5,5)]
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
