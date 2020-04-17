{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Asteroids

import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game

import           Control.Lens

main :: IO ()
main = play window windowColor fps world renderer eventhandler step
  where window = InWindow "Asteroids" (640, 480) (0, 0)
        windowColor  = black
        fps = 30
        world  = ( (640, 480)
                 , newShip & rotation .~ 30 & position .~ (50, 50) 
                 )
        renderer ((x, y), ship) = 
          color white $
            pictures  [ translate
                          (50 - fromIntegral (x `div` 2))
                          (50 - fromIntegral (y `div` 2)) 
                        . scale (0.5) (0.5)
                        $ text "Asteroids!"
                      , drawShip ship
                      ]
        eventhandler e = case e of
                           EventResize (x', y') -> _1 .~ (x', y')
                           _                    -> id
        step dt = _2 . rotation %~ (+(30*dt))
