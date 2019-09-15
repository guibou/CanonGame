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
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -Wall -Wno-missing-signatures #-}
module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import System.Random
import Control.Monad (when, replicateM_, guard)
import Data.Ecstasy
import Graphics.Gloss.Interface.IO.Game
import Control.Monad.IO.Class (liftIO)

import Lib

data World s = Entity
  { position :: Component s 'Field Point
  , velocity :: Component s 'Field Point
  , angle :: Component s 'Field Float
  , angularVelocity :: Component s 'Field Float
  , age :: Component s 'Field Float

  , enemy :: Component s 'Field ()
  , bullet :: Component s 'Field ()
  , gravity :: Component s 'Field ()
  , explosion :: Component s 'Field ()
  , particule :: Component s 'Field ()

  , player :: Component s 'Unique ()
  , score :: Component s 'Unique Int -- Note: apecs use a "global" store, we'll store the score on the player
  }
  deriving (Generic)

main = do
  (state, ()) <- yieldSystemT (0, defStorage) $ do
    _ <- initialize
    pure ()

  playIO
    FullScreen
    blue
    60
    state
    drawWrap
    eventWrap
    stepWrap
  where
    drawWrap state = do
      (_state', pics) <- yieldSystemT state doDraw
      pure pics

    eventWrap e state = do
      (state', ()) <- yieldSystemT state (handleEvent e)
      pure state'

    stepWrap t state = do
      (state', ()) <- yieldSystemT state (handleStep t)
      pure state'

initialize = do
  createEntity $ newEntity
    { player = Just ()
    , position = Just (0, 0)
    , velocity = Just (0, 0)
    , angle = Just 0
    , angularVelocity = Just 0
    , score = Just 0
    }

doDraw = do
  tank <- efor allEnts $ do
    query player
    a <- query angle
    (tankX, tankY) <- query position

    pure $ Translate tankX tankY $ Pictures
      [ drawTank
      , Translate (2*tankSize) (tankSize * 4/5) (Rotate a $ drawCanon)
      ]

  bullets <- efor allEnts $ do
    query bullet
    p <- query position

    pure $ drawProjectile p

  enemies <- efor allEnts $ do
    query enemy
    v <- query velocity
    p <- query position

    pure $ drawEnemy p v

  explosions <- efor allEnts $ do
    query explosion
    p <- query position
    a <- query age

    pure $ uncurry translate p $ drawExplosion a (fst p + snd p * a)

  particules <- efor allEnts $ do
    query particule
    p <- query position
    a <- query age

    pure $ color (withAlpha (1 - a / 5) red) $ uncurry translate p $ ThickCircle 1 3

  s <- efor allEnts $ query score

  let t = color white $ translate 0 (0.96 * screenY) $ scale 0.1 0.1 $ Text ("Score: " <> show (sum s))
  let pics = (explosions <> tank <> bullets <> enemies <> particules <> [t])

  liftIO $ print (length pics)

  pure (applyViewPortToPicture viewport $ Pictures pics)

handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) = emap allEnts $ do
  query player
  pure $ unchanged {
    velocity = Set (-100, 0)
    }
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) = emap allEnts $ do
  query player
  pure $ unchanged {
    velocity = Set (100, 0)
    }
handleEvent (EventKey (SpecialKey KeyLeft) Up _ _) = emap allEnts $ do
  query player
  pure $ unchanged {
    velocity = Set (0, 0)
    }
handleEvent (EventKey (SpecialKey KeyRight) Up _ _) = emap allEnts $ do
  query player
  pure $ unchanged {
    velocity = Set (0, 0)
    }
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) = emap allEnts $ do
  query player
  pure $ unchanged {
    angularVelocity = Set (-90)
    }
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) = emap allEnts $ do
  query player
  pure $ unchanged {
    angularVelocity = Set (90)
    }
handleEvent (EventKey (SpecialKey KeyUp) Up _ _) = emap allEnts $ do
  query player
  pure $ unchanged {
    angularVelocity = Set 0
    }
handleEvent (EventKey (SpecialKey KeyDown) Up _ _) = emap allEnts $ do
  query player
  pure $ unchanged {
    angularVelocity = Set 0
    }
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) = do
  e <- efor allEnts $ do
    query player
    tp <- query position
    a <- query angle
    pure $ mkProjectile tp a
  mapM_ createEntity e
  pure ()
handleEvent _ = pure ()

handleStep dt = do
  -- move object with linear velocity
  emap allEnts $ do
    (pX, pY) <- query position
    (vX, vY) <- query velocity
    pure $ unchanged {
      position = Set (pX + dt * vX, pY + dt * vY)
      }
  emap allEnts $ do
    a <- query angle
    dA <- query angularVelocity
    pure $ unchanged {
      angle = Set (a + dt * dA)
      }

  -- Apply gravity
  emap allEnts $ do
    (vX, vY) <- query velocity
    query gravity
    pure $ unchanged {
      velocity = Set (vX, vY - dt * 100)
      }

  -- Generate new enemies
  newEnemy

  -- Age everything
  emap allEnts $ do
    t <- query age
    pure $ unchanged {
      age = Set (t + dt)
      }

  -- Handle collisions
  en <- efor allEnts $ do
    query enemy
    posEnemy <- query position
    velEnemy <- query velocity
    e <- queryEnt

    pure (posEnemy, velEnemy, e)

  bul <- efor allEnts $ do
    query bullet
    posBullet <- query position
    e <- queryEnt

    pure (posBullet, e)

  flip (mapM_) bul $ \(posBullet, bulletEntity) -> do
    flip (mapM_) en $ \(posEnemy, velEnemy@(vxe, vye), enemyEntity) -> do
      when ((distance2 posEnemy posBullet) < 500) $ do
        deleteEntity bulletEntity
        deleteEntity enemyEntity

        emap allEnts $ do
          s <- query score
          pure $ unchanged {
            score = Set (s + 1)
            }

        _ <- createEntity $ newEntity
          { explosion = Just ()
          , position = Just posBullet
          , age = Just 0
          , velocity = Just velEnemy
          , gravity = Just ()
          }

        replicateM_ 10 $ do
          vx <- liftIO $ randomIO
          vy <- liftIO $ randomIO

          createEntity $ newEntity
            { particule = Just ()
            , position = Just posBullet
            , velocity = Just (vx * 200 + vxe, vy * 200 + vye)
            , age = Just 0
            , gravity = Just ()
            }

  -- clean old bullets
  oldBullets <- efor allEnts $ do
    query bullet
    (_, pY) <- query position
    guard $ pY < 0
    queryEnt

  mapM_ deleteEntity oldBullets

  -- clean old planes
  oldPlanes <- efor allEnts $ do
    query enemy
    (pX, _) <- query position
    guard $ (pX < -100 || pX > screenX + 100)
    queryEnt

  mapM_ deleteEntity oldPlanes

  -- clean old explosions and particules
  tooAged <- efor allEnts $ do
    a <- query age
    guard $ a > 5
    queryEnt

  mapM_ deleteEntity tooAged

mkProjectile t angle = do
  let (p, s) = newProjectile t angle
  newEntity
    { bullet = Just ()
    , position = Just p
    , velocity = Just s
    , gravity = Just ()
    }

newEnemy = do
  res <- liftIO mkEnemy
  case res of
    Just (p, v) -> do
      _ <- createEntity $ newEntity
        { enemy = Just ()
        , position = Just p
        , velocity = Just v
        }
      pure ()
    Nothing -> pure ()
