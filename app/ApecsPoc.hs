-- This doesn't work! HLS is broken. Can't find any symbols in this module.
-- Also, with all the monadic stuff, I'm NOT ready for Apecs!
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Apecs
import Control.Monad
import Data.Monoid
import Raylib.Types
import Raylib.Types.Core
import Raylib.Util.Math
import System.Exit
import System.Random


newtype Position = Position Vector2 deriving (Show)
instance Component Position where type Storage Position = Map Position


newtype Velocity = Velocity Vector2 deriving (Show)
instance Component Velocity where type Storage Velocity = Map Velocity


makeWorld "World" [''Position, ''Velocity]


type System' a = System World a


initialize :: System' ()
initialize = do
    _ <- newEntity (Position (Vector2 0 0), Velocity (Vector2 1 1))
    _ <- newEntity (Position (Vector2 10 10), Velocity (Vector2 (-1) (-1)))
    return ()


stepPosition :: Float -> System' ()
stepPosition dT = cmap $ \(Position p, Velocity v) -> Position (p |+| v |* dT)


main :: IO ()
main = do
    w <- initWorld
    runWith w $ do
        initialize
        forM_ [1 .. 10] $ \_ -> do
            p <- stepPosition 1
            liftIO $ print p -- prints ()