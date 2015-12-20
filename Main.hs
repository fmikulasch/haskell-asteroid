import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import State
import Game
import Settings
import Data.Fixed

main
 = play (InWindow "Astroid" (floor sWidth, floor sHeight) (100, 100))
        sBGColor
        sUpdate
        initialState
        drawState
        handleInput
        stepState


----- Rendering Functions -----

drawState :: State -> Picture
drawState (State ship asteroids bullets _ _)
    = Pictures
    $ drawShip ship
    : map drawAsteroid asteroids
    ++ map drawBullet bullets
    ++ stars

drawAsteroid :: Asteroid -> Picture
drawAsteroid (Asteroid _ (x,y) alpha _ size 0.0)
    = Color (dark white)
    $ Translate x y
    $ Rotate alpha
    $ asteroidPicture size

drawAsteroid (Asteroid _ pos  _ _ _ dead)
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
    = let x' = x `mod'` 3
          y' = y `mod'` 3 in
    Pictures
    [ Line [from,normalizeV from + from]
    | from <- [ a * b
              | a <- [(1,3),(4,1),(1,8),(5,5)]
              , b <- [(1,1),(-1,x'),(- y',- 0.4),(1,- y')]
              ]
    ]

stars :: [Picture]
stars = []
