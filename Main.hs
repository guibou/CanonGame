{-# LANGUAGE TypeApplications #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Juicy
import Data.Maybe
import Debug.Trace
import System.IO.Unsafe
import System.Random
import Data.Either

data Explosion = Explosion
  { explosionPosition :: Point
  , explosionAge :: Float
  }

data Projectile = Projectile
  { position :: Point
  , speed :: Point
  }

data World = World
  { tankPosition :: Point
  , tankDx :: Float
  , angle :: Float
  , dAngle :: Float
  , projectiles :: [Projectile]
  , generator :: StdGen
  , enemies :: [Enemy]
  , explosions :: [Explosion]
  }

initWorld = World
  { tankPosition = (0, 0)
  , tankDx = 0
  , angle = 0
  , dAngle = 0
  , projectiles = []
  , enemies = []
  , generator = mkStdGen 0
  , explosions = []
  }

viewport = viewPortInit
  { viewPortScale = 5
  , viewPortTranslate = (-screenX / 2, -screenY / 2)
  }

draw w = applyViewPortToPicture viewport $

  Pictures [
  (Translate tankX tankY $ Pictures
    [ drawTank
    , Translate (2*tankSize) (tankSize * 4/5) (Rotate (angle w) $ drawCanon 0)
    ]
  )
  , Pictures (map (\e -> uncurry translate (explosionPosition e) (drawExplosion (explosionAge e) (fst $ explosionPosition e))) (explosions w))
  , Pictures (map drawProjectile (projectiles w))
  , Pictures (map drawEnemy (enemies w))
  ]
  where
    (tankX, tankY) = tankPosition w

event (EventKey (SpecialKey KeyLeft) Down _ _) w = w { tankDx = -dx }
event (EventKey (SpecialKey KeyRight) Down _ _) w = w { tankDx = dx }
event (EventKey (SpecialKey KeyLeft) Up _ _) w = w { tankDx = 0 }
event (EventKey (SpecialKey KeyRight) Up _ _) w = w { tankDx = 0 }

event (EventKey (SpecialKey KeyUp) Down _ _) w = w { dAngle = -3 }
event (EventKey (SpecialKey KeyDown) Down _ _) w = w { dAngle = 3 }
event (EventKey (SpecialKey KeyUp) Up _ _) w = w { dAngle = 0 }
event (EventKey (SpecialKey KeyDown) Up _ _) w = w { dAngle = 0 }

event (EventKey (SpecialKey KeySpace) Down _ _) w = w
  { projectiles = mkProjectile w : projectiles w }

event _ w = w

killEnemy :: [Projectile] -> Enemy -> Either Explosion Enemy
killEnemy pr e
  | any (\pr -> distance2 (enemyPosition e) (position pr) < 500) pr = Left $ Explosion (enemyPosition e) 0
  | otherwise = Right e

distance2 (x, y) (x', y') = sq (x -x') + sq (y - y')
  where
    sq x = x * x

step dt w = let
  (newE, g') = newEnemy w
  projectiles' = mapMaybe (moveProjectile dt) (projectiles w)
  (explosions', enemies') = partitionEithers . map (killEnemy projectiles') $ maybe id (:) newE $ mapMaybe (moveEnemy dt) (enemies w)

  in
  w
  { tankPosition = ((fst $ tankPosition w) + tankDx w, snd $ tankPosition w)
  , angle = angle w + dAngle w
  , projectiles = projectiles'
  , enemies = enemies'
  , generator = g'
  , explosions = map (advanceExplosion dt) (explosions w) ++ explosions'
  }

advanceExplosion dt e = e {explosionAge = explosionAge e + dt}

moveProjectile dt projectile
  | snd newPos < 0 = Nothing
  | otherwise = Just $ Projectile newPos newSpeed
  where
    (posX, posY) = position projectile
    (speedX, speedY) = speed projectile

    newSpeed = (speedX, speedY - dt * 100)
    newPos = (posX + dt * speedX, posY + dt * speedY)

f16picture = Scale 0.05 0.05 $ case unsafePerformIO $ loadJuicy "f16.png" of
  Nothing -> error "Cannot load the picture"
  Just p -> p

main :: IO ()
main = do
  screenSize <- getScreenSize

  play
    FullScreen
    blue
    24
    initWorld
    draw
    event
    step

drawTank = Color black $ Polygon [
  (0, 0),
  (2 * tankSize, 0),
  (2 * tankSize, tankSize),
  (0, tankSize)
  ]

drawCanon canonAngle = Color red $ Polygon
  [ (tankSize, tankSize / 5)
  , (tankSize, 0)
  , (0, 0)
  , (0, tankSize / 5)
  ]

drawProjectile projectile = Translate pX pY $ Color white $ ThickCircle 2 5
  where
    (pX, pY) = position projectile

canonRadius = tankSize / 2

tankSize = 25

dx = 4
initSpeed = 11 * tankSize

screenX :: Float
screenY :: Float
(screenX, screenY) = (3840 / 5,2160 / 5)

mkProjectile :: World -> Projectile
mkProjectile w = Projectile p s
  where
    (tX, tY) = tankPosition w
    (cX, cY) = (2 * tankSize + tX, tankSize * 4/5 + tY)

    canonLength = tankSize
    a = - angle w * pi / 180
    (dX, dY) = (cos a, sin a)

    p = (cX + canonLength * dX, cY + canonLength * dY)

    s = (initSpeed * dX, initSpeed * dY)

data Enemy = Enemy
  { enemyPosition :: Point
  , enemySpeed :: Float
  }

drawEnemy :: Enemy -> Picture
drawEnemy e = translate ex ey $ Scale (if enemySpeed e < 0 then 1 else -1) 1 $ f16picture
  where
    (ex, ey) = enemyPosition e

moveEnemy :: Float -> Enemy -> Maybe Enemy
moveEnemy dt e
  | pX < -100 || pX > screenX + 100 = Nothing
  | otherwise = Just $ e
  {
    enemyPosition = (pX + enemySpeed e, pY)
  }
    where
      (pX, pY) = enemyPosition e

newEnemy :: World -> (Maybe Enemy, StdGen)
newEnemy w = let
  (addEnemy, g') = randomR (0 :: Int, 50) (generator w)
  (posY, g'') = randomR (150 :: Float, screenY) g'
  (speedX, g''') = randomR (planeSpeed, planeSpeed * 5) g''
  (onRight, g'''') = random g'''
  in
  if addEnemy /= 0
  then (Nothing, g''')
  else (
    Just (Enemy
          { enemyPosition = (if onRight then screenX else 0, posY)
          , enemySpeed = if onRight then -speedX else speedX
          }),
      g''')

planeSpeed = 1


drawExplosion age seed = let
  g = mkStdGen (truncate seed)

  n = 10
  radius = take n $ randoms @Float g
  angle = map (\i -> fromIntegral i / fromIntegral n * 2 * pi) [0..(fromIntegral n - 1)]

  radiusScale = 25

  in Color (withAlpha (1 - age / 5) orange) $ Polygon $ zipWith (\r a -> (radiusScale * (r + 0.5) * cos a, radiusScale * (r + 0.5) * sin a)) radius angle
