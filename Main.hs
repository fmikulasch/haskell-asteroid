import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import State
import Game

main
 = play (InWindow "Astroid" (floor width2, floor height2) (100, 100))
        black
        100
        initialState
        drawState
        handleInput
        stepState

width2, height2 :: Float
width2  = 700
height2 = 500

----- Rendering Functions -----

drawState :: State -> Picture
drawState (State ship asteroids bullets _ _)
    = Pictures
    $ drawShip ship
    : map drawAsteroid asteroids
    ++ map drawBullet bullets

drawAsteroid :: Asteroid -> Picture
drawAsteroid (Asteroid _ (x,y) size)
    = Color (dark white)
    $ Translate x y
    $ ThickCircle 1 size

drawBullet :: Bullet -> Picture
drawBullet (Bullet _ (x,y) _)
    = Color white
    $ Translate x y
    $ Circle 5

drawShip :: Ship -> Picture
drawShip (Ship _ (x,y) alpha _ _)
    = Color white
    $ Translate x y
    $ Rotate (-alpha) shipPicture

shipPicture :: Picture
shipPicture
    = Polygon
    [ (20,0)
    , (-10,10)
    , (-4,0)
    , (-10,-10)
    , (20,0)
    ]
