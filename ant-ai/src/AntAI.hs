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
import Debug.Pretty.Simple (pTraceShowId, pTraceShowM)
import Debug.Trace (traceShowId)
import DrawWalls (drawWallsSys)
import FlatlandRenderer (flatlandRendererSys, updateAntFR)
import Food (foodSys)
import GHC.Float (int2Float)
import Numeric.LinearAlgebra (Vector (..), fromList)
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
import Raylib.Types.Core (Color (..), Vector2 (..))
import Raylib.Util (drawing)
import Raylib.Util.Colors (darkBrown, green, lightGray, white)
import Shared (System (..), calcCenteredRect, gameLoop, getNextPos, mkAnt, rgbToLinear)
import System.Random (mkStdGen, randomIO, randomR)
import Types (
    Ant (..),
    AntDecision (..),
    Container (..),
    GoDir (..),
    Nest (..),
    Sprite (LeftSprite, RightSprite),
    VisionRay (..),
    WheelPos (Center, TurnLeft, TurnRight),
    World (..),
 )


initAntAIWorld :: IO World
initAntAIWorld = do
    playerAntSeed <- randomIO
    antSeeds <- replicateM 1 randomIO
    let antPos = Vector2 (int2Float screenWidth / 2) (int2Float screenHeight / 2)
        playerAnt = mkAnt antPos playerAntSeed
        ants = map (mkAnt antPos) antSeeds
        nest = Nest (Container 0 (calcCenteredRect antPos collisionRectSize))
        walls = []
    _ <- initWindow screenWidth screenHeight "Ant AI"
    setTargetFPS fps
    setMouseCursor MouseCursorCrosshair
    return $ World playerAnt ants nest False False False True walls Nothing [] Nothing []


handleAntAIInput :: World -> IO World
handleAntAIInput w = return w


antBrainForward :: Vector Float -> AntDecision
antBrainForward inputVector = GoForward


antBrainRandom :: Vector Float -> AntDecision
antBrainRandom inputVector = undefined


mkInputVector :: Ant -> Vector Float
mkInputVector ant =
    -- Flatten vision rgb colors to a single list of normalized floats
    -- Compass direction and distance to nest, normalized
    -- Has food? 1.0 or 0.0
    -- TODO Later, add a few random numbers to add jitter
    let visionRayColors =
            ant.aVisionRays
                & concatMap (\ray -> let (r, g, b, a) = rgbToLinear ray.rColor in [r, g, b])
        antNestAngle = ant.aNestAngle
        antNestDistance = ant.aNestDistance
        hasFood = if ant.aHasFood then 1.0 else 0.0
        _ = traceShowId [antNestAngle, antNestDistance, hasFood]
    in  fromList $ visionRayColors ++ [antNestAngle, antNestDistance, hasFood]


applyAntDecision :: AntDecision -> Ant -> Ant
applyAntDecision decision ant = case decision of
    GoLeft -> ant{aWheelPos = TurnLeft, aGoDir = Stop}
    GoForwardLeft -> ant{aWheelPos = TurnLeft, aGoDir = Forward}
    GoForward -> ant{aWheelPos = Center, aGoDir = Forward}
    GoForwardRight -> ant{aWheelPos = TurnRight, aGoDir = Forward}
    GoRight -> ant{aWheelPos = TurnRight, aGoDir = Stop}
    GoBackwardRight -> ant{aWheelPos = TurnRight, aGoDir = Backward}
    GoBackward -> ant{aWheelPos = Center, aGoDir = Backward}
    GoBackwardLeft -> ant{aWheelPos = TurnLeft, aGoDir = Backward}
    GoNowhere -> ant{aWheelPos = Center, aGoDir = Stop}


updateAntAIWorld :: World -> World
updateAntAIWorld w =
    let ants = w.wAnts
        antDecisions = ants & map (antBrainForward . mkInputVector)
    in  w
            { wAnts =
                ants
                    & zipWith applyAntDecision antDecisions
                    & map (updateAntFR w . updateAntMovement w)
            }


-- TODO Move this to Shared
drawAnt :: Color -> Ant -> IO ()
drawAnt color ant = do
    let antPos = ant.aPos
    drawCircleV antPos 5 color
    let antDir = antPos & getNextPos ant.aAngle 20
    drawLineEx antPos antDir 5 color


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
                allSystems.render w
                drawFPS 10 10
            }


driveAntAI :: IO ()
driveAntAI = initAntAIWorld >>= gameLoop antAISysWrapped windowShouldClose
