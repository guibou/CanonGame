{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -Wall -Wno-missing-signatures #-}
module Main
  ( main
    )
where

import Apecs
import Apecs.Gloss
import Control.Monad (replicateM_, void, when)
import Data.Monoid (Sum (..))
import Graphics.Gloss.Data.ViewPort
import Lib
import System.Random

-- Store definition
newtype Position = Position Point deriving (Show)

instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity Point deriving (Show)

instance Component Velocity where type Storage Velocity = Map Velocity

newtype Angle = Angle Float deriving (Show)

instance Component Angle where type Storage Angle = Map Angle

newtype AngularVelocity = AngularVelocity Float deriving (Show)

instance Component AngularVelocity where type Storage AngularVelocity = Map AngularVelocity

data Bullet = Bullet deriving (Show)

instance Component Bullet where type Storage Bullet = Map Bullet

data Gravity = Gravity deriving (Show)

instance Component Gravity where type Storage Gravity = Map Gravity

data Enemy = Enemy deriving (Show)

instance Component Enemy where type Storage Enemy = Map Enemy

data Explosion = Explosion deriving (Show)

instance Component Explosion where type Storage Explosion = Map Explosion

data Particule = Particule deriving (Show)

instance Component Particule where type Storage Particule = Map Particule

data Age = Age Float deriving (Show)

instance Component Age where type Storage Age = Map Age

data Player = Player deriving (Show)

instance Component Player where type Storage Player = Unique Player

newtype Score = Score (Sum Int)
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)

instance Component Score where type Storage Score = Global Score

type All = ((Position, Velocity, Player, Enemy, Bullet), (Angle, AngularVelocity, Gravity, Explosion, Age), (Particule))

makeWorld "World" [''Position, ''Velocity, ''Player, ''Enemy, ''Bullet, ''Angle, ''AngularVelocity, ''Gravity, ''Camera, ''Explosion, ''Age, ''Particule, ''Score]

-- Main
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

initialize = do
  void $ newEntity (Player, Position (0, 0), Velocity (0, 0), Angle 0, AngularVelocity 0)

-- Draw
doDraw = do
  tank <- foldDraw $ \(Player, Angle a, Position p) -> drawFullTank p a
  bullets <- foldDraw $ \(Bullet, Position p) -> drawProjectile p
  enemies <- foldDraw $ \(Enemy, Position p, Velocity v) -> drawEnemy p v
  explosions <- foldDraw $ \(Explosion, Position p, Age a) -> uncurry translate p $ drawExplosion a (fst p + snd p * a)
  particules <- foldDraw $ \(Particule, Position p, Age a) -> color (withAlpha (1 - a / 5) red) $ uncurry translate p $ ThickCircle 1 3
  Score (Sum s) <- get global
  let t = color white $ translate 0 (0.96 * screenY) $ scale 0.1 0.1 $ Text ("Score: " <> show s)
  let pics = explosions <> tank <> bullets <> enemies <> particules <> t
  pure (applyViewPortToPicture viewport $ pics)

-- Events
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) =
  cmap (\Player -> Velocity (-100, 0))
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) =
  cmap (\Player -> Velocity (100, 0))
handleEvent (EventKey (SpecialKey KeyLeft) Up _ _) =
  cmap (\Player -> Velocity (0, 0))
handleEvent (EventKey (SpecialKey KeyRight) Up _ _) =
  cmap (\Player -> Velocity (0, 0))
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) =
  cmap (\Player -> AngularVelocity (-90))
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) =
  cmap (\Player -> AngularVelocity 90)
handleEvent (EventKey (SpecialKey KeyUp) Up _ _) =
  cmap (\Player -> AngularVelocity 0)
handleEvent (EventKey (SpecialKey KeyDown) Up _ _) =
  cmap (\Player -> AngularVelocity 0)
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) = do
  cmapM_ $ \(Player, Position tp, Angle a) -> do
    let (p, s) = newProjectile tp a
    void $ newEntity (Bullet, Position p, Velocity s, Gravity)
handleEvent _ = pure ()

-- Step
handleStep dt = do
  -- Move objects with linear velocity
  cmap (\(Position (pX, pY), Velocity (vX, vY)) -> Position (pX + dt * vX, pY + dt * vY))
  -- Move objects with angular velocity
  cmap (\(Angle a, AngularVelocity dA) -> Angle (a + dt * dA))
  -- Apply gravity
  cmap (\(Velocity (vX, vY), Gravity) -> Velocity (vX, vY - dt * 100))
  -- Generate new enemies
  res <- liftIO mkEnemy
  case res of
    Just (p, v) -> do
      void $ newEntity (Enemy, Position p, Velocity v)
    Nothing -> pure ()
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
          newEntity (Particule, Position posBullet, Velocity (vx * 200 + vxe, vy * 200 + vye), Age 0, Gravity)
  -- clean old bullets
  cmap $ \ent@(Bullet, Position (_, pY), Gravity) ->
    if pY < 0
      then Left (Not @All)
      else Right ent
  -- clean old planes
  cmap $ \ent@(Enemy, Position (pX, _)) ->
    if pX < -100 || pX > screenX + 100
      then Left (Not @All)
      else Right ent
  -- clean old explosions and particules
  cmap $ \ent@(Age x) ->
    if x > 5
      then Left (Not @All)
      else Right ent
