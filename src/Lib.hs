{-# LANGUAGE TypeApplications #-}

module Lib where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import System.IO.Unsafe
import Graphics.Gloss.Juicy

drawEnemy (ex, ey) speed = translate ex ey $ Scale (if fst speed < 0 then 1 else -1) 1 $ f16picture

planeSpeed = 20 :: Float

drawExplosion age seed = let
  g = mkStdGen (truncate seed)

  n = 10
  radius = take n $ randoms @Float g
  angle = map (\i -> fromIntegral i / fromIntegral n * 2 * pi) [0 :: Int ..(fromIntegral n - 1)]

  radiusScale = 25

  in Color (withAlpha (1 - age / 5) orange) $ Polygon $ zipWith (\r a -> (radiusScale * (r + 0.5) * cos a, radiusScale * (r + 0.5) * sin a)) radius angle

drawTank = Color black $ Polygon [
  (0, 0),
  (2 * tankSize, 0),
  (2 * tankSize, tankSize),
  (0, tankSize)
  ]

drawCanon = Color red $ Polygon
  [ (tankSize, tankSize / 5)
  , (tankSize, 0)
  , (0, 0)
  , (0, tankSize / 5)
  ]

drawProjectile (pX, pY) = Translate pX pY $ Color white $ ThickCircle 2 5

tankSize = 25

initSpeed = 11 * tankSize

screenX :: Float
screenY :: Float
(screenX, screenY) = (3840 / 5,2160 / 5)

f16picture = Scale 0.05 0.05 $ case unsafePerformIO $ loadJuicy "f16.png" of
  Nothing -> error "Cannot load the picture"
  Just p -> p

viewport = viewPortInit
  { viewPortScale = 5
  , viewPortTranslate = (-screenX / 2, -screenY / 2)
  }

distance2 (x, y) (x', y') = sq (x -x') + sq (y - y')
  where
    sq v = v * v

drawFullTank (tankX, tankY) a =
    Translate tankX tankY $ Pictures
                                                                    [ drawTank
                                                                    , Translate (2*tankSize) (tankSize * 4/5) (Rotate a $ drawCanon)
                                                                    ]

newProjectile (tX, tY) angle =
  let
    (cX, cY) = (2 * tankSize + tX, tankSize * 4/5 + tY)

    canonLength = tankSize
    a = - angle * pi / 180
    (dX, dY) = (cos a, sin a)

    p = (cX + canonLength * dX, cY + canonLength * dY)

    s = (initSpeed * dX, initSpeed * dY)
  in (p, s)

mkEnemy = do
  addEnemy <- randomRIO (0 :: Int, 50)
  posY <- randomRIO (150 :: Float, screenY)
  speedX <- randomRIO (planeSpeed, planeSpeed * 5)
  onRight <- randomIO

  if addEnemy == 0
    then pure $ Just (if onRight then (screenX, posY) else (0, posY), if onRight then (-speedX, 0 :: Float) else (speedX, 0))
    else pure Nothing
