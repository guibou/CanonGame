{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS -Wall -Wno-missing-signatures #-}
module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Juicy
import System.IO.Unsafe
import System.Random
import Apecs
import Apecs.Core
import Apecs.Gloss
import Control.Monad (when, replicateM_)
import Data.IORef
import Data.Monoid (Sum(..))

newtype Position = Position Point deriving Show
instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity Point deriving Show
instance Component Velocity where type Storage Velocity = Map Velocity

newtype Angle = Angle Float deriving Show
instance Component Angle where type Storage Angle = Map Angle

newtype AngularVelocity = AngularVelocity Float deriving Show
instance Component AngularVelocity where type Storage AngularVelocity = Map AngularVelocity

data Bullet = Bullet deriving Show
instance Component Bullet where type Storage Bullet = Map Bullet

data Gravity = Gravity deriving Show
instance Component Gravity where type Storage Gravity = Map Gravity

data Enemy = Enemy deriving Show
instance Component Enemy where type Storage Enemy = Map Enemy

data Explosion = Explosion deriving Show
instance Component Explosion where type Storage Explosion = Map Explosion

data Particule = Particule deriving Show
instance Component Particule where type Storage Particule = Map Particule

data Age = Age Float deriving Show
instance Component Age where type Storage Age = Map Age

data Player = Player deriving Show
instance Component Player where type Storage Player = Unique Player

newtype Score = Score (Sum Int)
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)
instance Component Score where type Storage Score = Global Score

type All = ((Position, Velocity, Player, Enemy, Bullet), (Angle, AngularVelocity, Gravity, Explosion, Age), (Particule))
makeWorld "World" [''Position, ''Velocity, ''Player, ''Enemy, ''Bullet, ''Angle, ''AngularVelocity, ''Gravity, ''Camera, ''Explosion, ''Age, ''Particule, ''Score]

type System' a = System World a

initialize :: System' ()
initialize = do
  _player <- newEntity (Player, Position (0, 0), Velocity (0, 0), Angle 0, AngularVelocity 0)
  pure ()

viewport = viewPortInit
  { viewPortScale = 5
  , viewPortTranslate = (-screenX / 2, -screenY / 2)
  }

doDraw :: System' Picture
doDraw = do
  i <- liftIO $ newIORef (0 :: Int)
  let
    foldDraw' :: forall t. (Has World IO t,
                            Apecs.Core.ExplGet IO (Storage t),
                            ExplMembers IO (Storage t)
                           ) => (t -> Picture) -> System World Picture
    foldDraw' f = foldDrawM $ \x -> do
      liftIO $ modifyIORef' i (+1)
      pure (f x)

  tank <- foldDraw' $ \(Player, Angle a, Position (tankX, tankY)) -> Translate tankX tankY $ Pictures
                                                           [ drawTank
                                                           , Translate (2*tankSize) (tankSize * 4/5) (Rotate a $ drawCanon)
                                                           ]
  bullets <- foldDraw' $ \(Bullet, Position p) -> drawProjectile p
  enemies <- foldDraw' $ \(Enemy, Position p, Velocity v) -> drawEnemy p v
  explosions <- foldDraw' $ \(Explosion, Position p, Age a) -> uncurry translate p $ drawExplosion a (fst p + snd p * a)
  particules <- foldDraw' $ \(Particule, Position p, Age a) -> color (withAlpha (1 - a / 5) red) $ uncurry translate p $ ThickCircle 1 3

  iRes <- liftIO $ readIORef i
  liftIO $ print iRes

  Score (Sum s) <- get global

  let t = color white $ translate 0 (0.96 * screenY) $ scale 0.1 0.1 $ Text ("Score: " <> show s)

  pure (applyViewPortToPicture viewport $ (explosions <> tank <> bullets <> enemies <> particules <> t))

handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) = cmap (\(Player, Velocity (_, _)) -> Velocity (-100, 0))
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) = cmap (\(Player, Velocity (_, _)) -> Velocity (100, 0))
handleEvent (EventKey (SpecialKey KeyLeft) Up _ _) = cmap (\(Player, Velocity (_, _)) -> Velocity (0, 0))
handleEvent (EventKey (SpecialKey KeyRight) Up _ _) = cmap (\(Player, Velocity (_, _)) -> Velocity (0, 0))

handleEvent (EventKey (SpecialKey KeyUp) Down _ _) = cmap (\(Player, AngularVelocity _) -> AngularVelocity (-90))
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) = cmap (\(Player, AngularVelocity _) -> AngularVelocity 90)
handleEvent (EventKey (SpecialKey KeyUp) Up _ _) = cmap (\(Player, AngularVelocity _) -> AngularVelocity 0)
handleEvent (EventKey (SpecialKey KeyDown) Up _ _) = cmap (\(Player, AngularVelocity _) -> AngularVelocity 0)
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) = do
  cmapM_ $ \(Player, Position tp, Angle a) -> do
    mkProjectile tp a
handleEvent _ = pure ()

distance2 (x, y) (x', y') = sq (x -x') + sq (y - y')
  where
    sq v = v * v

handleStep dt = do
  -- Move objects with linear velocity
  cmap (\(Position (pX, pY), Velocity (vX, vY)) -> Position (pX + dt * vX, pY + dt * vY))
  cmap (\(Angle a, AngularVelocity dA) -> Angle (a + dt * dA))

  -- Apply gravity
  cmap (\(Velocity (vX, vY), Gravity) -> Velocity (vX, vY - dt * 100))

  -- Generate new enemies
  newEnemy

  -- Age everything
  cmap $ \(Age t) -> Age (t + dt)

  -- Handle collisions
  cmapM_ $ \(Enemy, Position posEnemy, eEnemy, Velocity velEnemy@(vxe, vye)) -> do
    cmapM_ $ \(Bullet, Position posBullet, eBullet) -> do
      when ((distance2 posEnemy posBullet) < 500) $ do
        destroy eBullet (Proxy @All)
        destroy eEnemy (Proxy @All)
        modify global $ \(Score x) -> Score (x + 1)
        _ex <- newEntity (Explosion, Position posBullet, Age 0, Velocity velEnemy, Gravity)
        replicateM_ 10 $ do
          vx <- liftIO $ randomIO
          vy <- liftIO $ randomIO

          newEntity (Particule, Position posBullet, Velocity (vx * 200 + vxe,vy * 200 + vye), Age 0, Gravity)

  -- clean old bullets
  cmap $ \ent@(Bullet, Position (_, pY), Velocity _, Gravity) -> if pY < 0
                                            then Nothing
                                            else Just ent

  -- clean old planes
  cmap $ \ent@(Enemy, Position (pX, _)) -> if pX < -100 || pX > screenX + 100
                                                       then Left (Not @All)
                                                       else Right ent

  -- clean old explosions and particules
  cmap $ \ent@(Age x) -> if x > 10
                         then Left (Not @All)
                         else Right ent


f16picture = Scale 0.05 0.05 $ case unsafePerformIO $ loadJuicy "f16.png" of
  Nothing -> error "Cannot load the picture"
  Just p -> p

main :: IO ()
main = do
  w <- initWorld

  runWith w $ do
    initialize
    Apecs.Gloss.play
      FullScreen
      blue
      60
      doDraw
      handleEvent
      handleStep

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


mkProjectile :: Point -> Float -> System' ()
mkProjectile (tX, tY) angle = do
  _ <- newEntity (Bullet, Position p, Velocity s, Gravity)
  pure ()
  where
    (cX, cY) = (2 * tankSize + tX, tankSize * 4/5 + tY)

    canonLength = tankSize
    a = - angle * pi / 180
    (dX, dY) = (cos a, sin a)

    p = (cX + canonLength * dX, cY + canonLength * dY)

    s = (initSpeed * dX, initSpeed * dY)

drawEnemy (ex, ey) speed = translate ex ey $ Scale (if fst speed < 0 then 1 else -1) 1 $ f16picture

newEnemy :: System' ()
newEnemy = do
  addEnemy <- liftIO $ randomRIO (0 :: Int, 50)
  posY <- liftIO $ randomRIO (150 :: Float, screenY)
  speedX <- liftIO $ randomRIO (planeSpeed, planeSpeed * 5)
  onRight <- liftIO $ randomIO

  when (addEnemy == 0) $ do
    _ <- newEntity (Enemy, Position (if onRight then (screenX, posY) else (0, posY)), Velocity (if onRight then (-speedX, 0) else (speedX, 0)))
    pure ()

planeSpeed = 20

drawExplosion age seed = let
  g = mkStdGen (truncate seed)

  n = 10
  radius = take n $ randoms @Float g
  angle = map (\i -> fromIntegral i / fromIntegral n * 2 * pi) [0 :: Int ..(fromIntegral n - 1)]

  radiusScale = 25

  in Color (withAlpha (1 - age / 5) orange) $ Polygon $ zipWith (\r a -> (radiusScale * (r + 0.5) * cos a, radiusScale * (r + 0.5) * sin a)) radius angle
