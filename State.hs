-- | Game state
module State where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Settings

-- | The game state.
data State
        = State
        { stateShip      :: Ship
        , stateAsteroids :: [Asteroid]
        , stateBullets   :: [Bullet]
        , stateEffects   :: [Effect]
        , stateKeys      :: [Key]
        , randGen        :: StdGen
        }

data Ship
        = Ship
        { shipVelocity   :: Vector -- Is a Tuple (x,y), defined in Gloss
        , shipPosition   :: Point  -- Point = Vector
        , shipRotation   :: Float
        , shipReload     :: Float
        , shipExploding  :: Float
        }

data Effect
        = Pulse
        { pulsePosition  :: Point
        , pulseAlpha     :: Float
        , pulseTime      :: Float
        }

data Asteroid
        = Asteroid
        { asteroidVelocity  :: Vector
        , asteroidPosition  :: Point
        , asteroidRotation  :: Float
        , asteroidMomentum  :: Float
        , asteroidSize      :: Float
        , asteroidExploding :: Float
        }

data Bullet
        = Bullet
        { bulletVelocity :: Vector
        , bulletPosition :: Point
        , bulletLife     :: Float
        }

initialState :: State
initialState
        = State initialShip
                []
                []
                []
                []
                (mkStdGen sRandGen)

    where initialShip = Ship (0,0)
                             (0,0)
                             (0 :: Float)
                             0
                             0
