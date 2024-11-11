{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use guards" #-}
{-# LANGUAGE OverloadedRecordDot #-}

module AntAI where

import Control.Monad (forM, forM_, replicateM, when)
import Data.Fixed (mod')
import Data.Function ((&))
import Data.List (foldl')

-- import Debug.Trace (trace, traceShow)

import AI.HNN.FF.Network
import AntMovement (antMovementSys, updateAntMovement)
import Constants (
    antAcceleration,
    antJitterAngle,
    antMaxSpeed,
    antScale,
    antTurnAngle,
    borderWallThickness,
    collisionRectSize,
    fps,
    numAnts,
    screenHeight,
    screenWidth,
 )
import DrawWalls (drawWallsSys)
import FlatlandRenderer (flatlandRendererSys, updateAntFR)
import Food (foodSys)
import GHC.Float (int2Float)
import Numeric.LinearAlgebra (Vector (..))
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
import Raylib.Core.Shapes (drawCircleV, drawLineEx)
import Raylib.Core.Text (drawFPS)
import Raylib.Types (
    Color,
    KeyboardKey (..),
    MouseCursor (MouseCursorCrosshair),
 )
import Raylib.Types.Core (Vector2 (..))
import Raylib.Util (drawing)
import Raylib.Util.Colors (darkBrown, green, lightGray, white)
import Shared (System (..), calcCenteredRect, gameLoop, getNextPos, mkAnt)
import System.Random (mkStdGen, randomIO, randomR)
import Types (
    Ant (..),
    AntDecision (..),
    Container (..),
    GoDir (..),
    Nest (..),
    Sprite (LeftSprite, RightSprite),
    WheelPos (Center, TurnLeft, TurnRight),
    World (..),
 )


initAntAIWorld :: IO World
initAntAIWorld = do
    playerAntSeed <- randomIO
    antSeeds <- replicateM 10 randomIO
    let antPos' = Vector2 (int2Float screenWidth / 2) (int2Float screenHeight / 2)
        playerAnt = mkAnt antPos' playerAntSeed
        ants = map (mkAnt antPos') antSeeds
        nest = Nest (Container 0 (calcCenteredRect antPos' collisionRectSize))
        walls = []
    _ <- initWindow screenWidth screenHeight "Ant AI"
    setTargetFPS fps
    setMouseCursor MouseCursorCrosshair
    return $ World playerAnt ants nest False False False True walls Nothing [] Nothing []


handleAntAIInput :: World -> IO World
handleAntAIInput w = return w


antBrainForward :: Ant -> AntDecision
antBrainForward _ = GoForward


antBrainRandom :: Ant -> AntDecision
antBrainRandom ant = undefined


applyAntDecision :: AntDecision -> Ant -> Ant
applyAntDecision decision ant = case decision of
    GoLeft -> ant{antWheelPos = TurnLeft, antGoDir = Stop}
    GoForwardLeft -> ant{antWheelPos = TurnLeft, antGoDir = Forward}
    GoForward -> ant{antWheelPos = Center, antGoDir = Forward}
    GoForwardRight -> ant{antWheelPos = TurnRight, antGoDir = Forward}
    GoRight -> ant{antWheelPos = TurnRight, antGoDir = Stop}
    GoBackwardRight -> ant{antWheelPos = TurnRight, antGoDir = Backward}
    GoBackward -> ant{antWheelPos = Center, antGoDir = Backward}
    GoBackwardLeft -> ant{antWheelPos = TurnLeft, antGoDir = Backward}
    GoNowhere -> ant{antWheelPos = Center, antGoDir = Stop}


updateAntAIWorld :: World -> World
updateAntAIWorld w =
    let ants = w.wAnts
        antDecisions = map antBrainForward ants
        ants' =
            ants
                & zipWith applyAntDecision antDecisions
                & map (updateAntFR w . updateAntMovement w)
    in  w{wAnts = ants'}


-- TODO Move this to Shared
drawAnt :: Color -> Ant -> IO ()
drawAnt color ant = do
    let antPos' = ant.antPos
    drawCircleV antPos' 5 color
    let antDir = getNextPos ant.antAngle 20 antPos'
    drawLineEx antPos' antDir 5 color


renderAntAIWorld :: World -> IO ()
renderAntAIWorld w = forM_ w.wAnts (drawAnt darkBrown)


antAISys :: System World
antAISys = System handleAntAIInput updateAntAIWorld renderAntAIWorld


antAISysWrapped :: System World
antAISysWrapped =
    let allSystems =
            antAISys
                <> drawWallsSys
                <> pheromoneSys
                <> foodSys
                <> antMovementSys
                <> flatlandRendererSys
    in  allSystems
            { render = \w -> drawing $ do
                f11Pressed <- isKeyPressed KeyF11
                when f11Pressed toggleFullscreen
                clearBackground lightGray
                render allSystems w
                drawFPS 10 10
            }


driveAntAI :: IO ()
driveAntAI = initAntAIWorld >>= gameLoop antAISysWrapped windowShouldClose
