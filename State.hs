{-# LANGUAGE TemplateHaskell #-}

-- | Game state
module State where

import Control.Lens
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Settings


data Ship
        = Ship
        { _shipVelocity   :: Vector -- Is a Tuple (x,y), defined in Gloss
        , _shipPosition   :: Point  -- Point = Vector
        , _shipRotation   :: Float
        , _shipReload     :: Float
        , _shipExploding  :: Float
        }

data Effect
        = Pulse
        { _pulsePosition  :: Point
        , _pulseAlpha     :: Float
        , _pulseTime      :: Float
        }

data Asteroid
        = Asteroid
        { _asteroidVelocity  :: Vector
        , _asteroidPosition  :: Point
        , _asteroidRotation  :: Float
        , _asteroidMomentum  :: Float
        , _asteroidSize      :: Float
        , _asteroidExploding :: Float
        , _asteroidShape     :: Int
        }

data Bullet
        = Bullet
        { _bulletVelocity :: Vector
        , _bulletPosition :: Point
        , _bulletLife     :: Float
        }


-- | The game state.
data State
        = State
        { _stateType      :: Gametype
        , _stateShip      :: Ship
        , _stateAsteroids :: [Asteroid]
        , _stateBullets   :: [Bullet]
        , _stateEffects   :: [Effect]
        , _stateKeys      :: [Key]
        , _randGen        :: StdGen
        }

data Gametype
        = Menu
        | Game
        deriving (Eq)

makeLenses ''Ship
makeLenses ''Effect
makeLenses ''Asteroid
makeLenses ''Bullet
makeLenses ''State

menuState, initialState :: State
menuState
        = set stateType Menu initialState

initialState
        = State Game
                initialShip
                []
                []
                []
                []
                (mkStdGen sRandGen)

    where initialShip = Ship (0,0)
                             (0,0)
                             90
                             0
                             0
