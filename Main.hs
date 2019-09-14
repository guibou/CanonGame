module Main where

import Graphics.Gloss

data World = World

initWorld = World

draw World = Blank

event _ World = World

stop _ World = World

main :: IO ()
main = play
  FullScreen
  blue
  24
  initWorld
  draw
  event
  stop
