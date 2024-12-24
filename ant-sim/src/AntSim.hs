{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use guards" #-}
{-# LANGUAGE OverloadedRecordDot #-}

module AntSim where

import Control.Monad (when)
import Data.Fixed (mod')
import Data.Function ((&))
import Data.List (foldl')

import Data.Sequence (Seq, (<|), (><), (|>))
import Data.Sequence qualified as Seq

import AntMovement (antMovementSys)
import Constants (
    antAcceleration,
    antJitterAngle,
    antMaxSpeed,
    antScale,
    antTurnAngle,
    borderWallThickness,
    collisionRectSize,
    fps,
    screenHeight,
    screenWidth,
 )
import DrawWalls (drawWallsSys)
import FlatlandRenderer (flatlandRendererSys)
import Food (foodSys)
import GHC.Float (int2Float)
import Pheromones (pheromoneSys)
import Raylib.Core (
    clearBackground,
    initWindow,
    isKeyDown,
    isKeyPressed,
    setMouseCursor,
    setTargetFPS,
    setTraceLogLevel,
    toggleFullscreen,
    windowShouldClose,
 )
import Raylib.Core.Text (drawFPS)
import Raylib.Types (
    KeyboardKey (..),
    MouseCursor (MouseCursorCrosshair),
 )
import Raylib.Types.Core (Vector2 (..))
import Raylib.Util (drawing)
import Raylib.Util.Colors (lightGray, white)
import Shared (System (..), calcCenteredRect, gameLoop, mkAnt)
import System.Random (mkStdGen, randomIO, randomR)
import Types (
    Ant (..),
    Container (..),
    GoDir (..),
    Nest (..),
    Sprite (LeftSprite, RightSprite),
    WheelPos (Center, TurnLeft, TurnRight),
    World (..),
 )


initWorld :: IO World
initWorld = do
    seed <- randomIO
    let antPos = Vector2 (int2Float screenWidth / 2) (int2Float screenHeight / 2)
        playerAnt = mkAnt antPos seed
        nest = Nest (Container 0 (calcCenteredRect antPos collisionRectSize))
    _ <- initWindow screenWidth screenHeight "Haskell Ant Sim"
    setTargetFPS fps
    setMouseCursor MouseCursorCrosshair
    return $ World playerAnt Seq.empty nest True False False True Seq.empty Nothing Seq.empty Nothing Seq.empty


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
