{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use guards" #-}
{-# LANGUAGE OverloadedRecordDot #-}

module AntSim where

import Control.Monad (when)

import AntMovement (antMovementSys)
import Constants (
    fps,
    screenHeight,
    screenWidth,
 )
import DrawWalls (drawWallsSys)
import FlatlandRenderer (flatlandRendererSys)
import Food (foodSys)
import Pheromones (pheromoneSys)
import Raylib.Core (
    clearBackground,
    initWindow,
    isKeyPressed,
    setMouseCursor,
    setTargetFPS,
    toggleFullscreen,
    windowShouldClose,
 )
import Raylib.Core.Text (drawFPS)
import Raylib.Types (
    KeyboardKey (..),
    MouseCursor (MouseCursorCrosshair),
 )
import Raylib.Util (drawing)
import Raylib.Util.Colors (lightGray)
import Shared (System (..), defaultWorld, gameLoop)
import System.Random (randomIO)
import Types (
    World (..),
 )


initWorld :: IO World
initWorld = do
    _ <- initWindow screenWidth screenHeight "Haskell Ant Sim"
    setTargetFPS fps
    setMouseCursor MouseCursorCrosshair
    seed <- randomIO
    return (defaultWorld seed)


antSimSys :: System World
antSimSys =
    let allSystems =
            drawWallsSys
                <> pheromoneSys
                <> foodSys
                <> antMovementSys
                <> flatlandRendererSys
    in  allSystems
            { render = \w -> drawing $ do
                f11Pressed <- isKeyPressed KeyF11
                when f11Pressed toggleFullscreen
                clearBackground lightGray
                allSystems.render w
                drawFPS 10 10
            }


main :: IO ()
main = initWorld >>= gameLoop antSimSys windowShouldClose
