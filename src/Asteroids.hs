{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE InstanceSigs        #-}

module Asteroids where

import           Graphics.Gloss
import           Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic   as Pt

import           Control.Lens
import           Control.Applicative

data GameState =
  GameState { _playerShip :: Ship
            , _asteroids :: [Asteroid]
            , _projectiles :: [Projectile]
            }

data Ship =
  Ship { _shipPosition     :: Point
       , _shipVelocity     :: Vector
       , _shipRotation     :: Float
       , _shipAccelerating :: Bool
       , _shipAcceleration :: Float
       }

data Asteroid = 
  Asteroid { _asteroidPosition :: Point
           , _asteroidVelocity :: Vector
           , _asteroidRotation :: Float
           , _asteroidSize     :: ASize
           }

data ASize = Small | Medium | Large

data Projectile =
  Projectile { _projectilePosition :: Point
             , _projectileVelocity :: Vector
             , _projectileRotation :: Float
             }

makeLenses ''GameState
makeLenses ''Ship
makeLenses ''Asteroid
makeLenses ''Projectile

class GameObject a where
  position :: Lens' a Point
  velocity :: Lens' a Vector
  rotation :: Lens' a Float

instance GameObject Ship where
  position = shipPosition
  velocity = shipVelocity
  rotation = shipRotation

instance GameObject Asteroid where
  position = asteroidPosition
  velocity = asteroidVelocity
  rotation = asteroidRotation

instance GameObject Projectile where
  position = projectilePosition
  velocity = projectileVelocity
  rotation = projectileRotation

newShip :: Ship
newShip =
  Ship { _shipPosition     = (0, 0)
       , _shipVelocity     = (0, 0)
       , _shipRotation     = 0
       , _shipAccelerating = False
       , _shipAcceleration = 0
       }

drawShip :: Ship -> Picture
drawShip ship = translate x y 
              $ rotate angle 
              $ pictures [{-circle 2
                         ,-} line [(-15, -13), (0, 26), (15, -13), (-15, -13)]
                         ]
  where (x, y) = ship ^. position
        angle  = ship ^. rotation