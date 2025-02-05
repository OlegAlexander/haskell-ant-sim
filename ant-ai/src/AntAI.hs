{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use guards" #-}
{-# LANGUAGE OverloadedRecordDot #-}

module AntAI where

import Control.Monad (forM_, replicateM, when)
import Data.Function ((&))
import Data.Vector (Vector)
import Data.Vector qualified as V

-- import AI.HNN.FF.Network
import AntMovement (antMovementSys, updateAntMovement)
import Constants (
    collisionRectSize,
    foodColor,
    fps,
    numAnts,
    screenHeight,
    screenWidth,
 )
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Debug.Pretty.Simple (pTraceShowId, pTraceShowM)
import Debug.Trace (traceShowId)
import DrawWalls (drawWallsSys)
import FlatlandRenderer (flatlandRendererSys, updateAntFR)
import Food (foodSys)
import GHC.Float (int2Float)
import NeuralNetwork
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
import Raylib.Core.Shapes (drawCircleV, drawLineEx)
import Raylib.Core.Text (drawFPS, drawText)
import Raylib.Types (
    Color,
    KeyboardKey (..),
    MouseCursor (MouseCursorCrosshair),
 )
import Raylib.Types.Core (Vector2 (..))
import Raylib.Util (drawing)
import Raylib.Util.Colors (black, brown, darkBrown, lightGray, red)
import Shared (System (..), calcCenteredRect, gameLoop, getNextPos, mkAnt, rgbToLinear)
import System.Random (mkStdGen, randomIO)
import Types (
    Ant (..),
    AntDecision (..),
    Container (..),
    GoDir (..),
    Nest (..),
    TrainingMode (..),
    VisionRay (..),
    WheelPos (Center, TurnLeft, TurnRight),
    World (..),
 )


initAntAIWorld :: IO World
initAntAIWorld = do
    playerAntSeed <- randomIO
    antSeeds <- replicateM numAnts randomIO
    let antPos = Vector2 (int2Float screenWidth / 2) (int2Float screenHeight / 2)
        playerAnt = mkAnt antPos playerAntSeed
        ants = Seq.fromList (map (mkAnt antPos) antSeeds)
        nest = Nest (Container 0 (calcCenteredRect antPos collisionRectSize))
    _ <- initWindow screenWidth screenHeight "Ant AI"
    setTargetFPS fps
    setMouseCursor MouseCursorCrosshair
    return $ World playerAnt ants nest False False False True Seq.empty Nothing Seq.empty Nothing Seq.empty Off 0 0


handleAntAIInput :: World -> IO World
handleAntAIInput w = return w


antBrainForward :: Vector Float -> AntDecision
antBrainForward inputVector = GoForward


antBrainRandom :: Vector Float -> AntDecision
antBrainRandom inputVector =
    let randomDecision = case inputVector V.!? (V.length inputVector - 1) of
            Just value -> value & (* 4) & round & toEnum
            Nothing -> toEnum 0 -- This should never happen
    in  randomDecision


antBrainNeuralNetwork :: [Layer] -> Vector Float -> AntDecision
antBrainNeuralNetwork neuralNetwork inputVector =
    inputVector & forwardAll sigmoid neuralNetwork & V.maxIndex & toEnum


mkInputVector :: Ant -> Vector Float
mkInputVector ant =
    -- Flatten vision rgb colors to a single list of normalized floats
    -- Compass direction and distance to nest, normalized
    -- Has food? 1.0 or 0.0
    -- Add a random number to add jitter
    let visionRayColors =
            ant.aVisionRays
                & concatMap (\ray -> let (r, g, b, a) = rgbToLinear ray.rColor in [r, g, b])
        antNestAngle = ant.aNestAngle
        antNestDistance = ant.aNestDistance
        antRandomNoise = ant.aRandomNoise
        hasFood = if ant.aHasFood then 1.0 else 0.0
        inputVector = visionRayColors ++ [antNestAngle, antNestDistance, hasFood, antRandomNoise]
    in  -- _ = traceShowId (length inputVector)
        V.fromList inputVector


applyAntDecision :: AntDecision -> Ant -> Ant
applyAntDecision decision ant = case decision of
    GoLeft -> ant{aWheelPos = TurnLeft, aGoDir = Stop}
    GoForwardLeft -> ant{aWheelPos = TurnLeft, aGoDir = Forward}
    GoForward -> ant{aWheelPos = Center, aGoDir = Forward}
    GoForwardRight -> ant{aWheelPos = TurnRight, aGoDir = Forward}
    GoRight -> ant{aWheelPos = TurnRight, aGoDir = Stop}


-- GoBackwardRight -> ant{aWheelPos = TurnRight, aGoDir = Backward}
-- GoBackward -> ant{aWheelPos = Center, aGoDir = Backward}
-- GoBackwardLeft -> ant{aWheelPos = TurnLeft, aGoDir = Backward}
-- GoNowhere -> ant{aWheelPos = Center, aGoDir = Stop}

updateAntAIWorld :: World -> World
updateAntAIWorld w =
    let ants = w.wAnts
        antDecisions =
            ants & fmap (\ant -> antBrainNeuralNetwork ant.aBrain (mkInputVector ant))
    in  w
            { wAnts =
                ants
                    & Seq.zipWith applyAntDecision antDecisions
                    & fmap (updateAntFR w . updateAntMovement w)
            }


-- TODO Move this to Shared
-- TODO Draw ants on top of all other objects
drawAnt :: Color -> Ant -> IO ()
drawAnt color ant = do
    let antPos = ant.aPos
    -- If the ant has food, draw a piece of food in its mouth
    when ant.aHasFood $ do
        let foodPiecePos = getNextPos ant.aAngle 20 antPos
        drawCircleV foodPiecePos 10 foodColor
    -- Draw the ant
    drawCircleV antPos 5 color
    let antDir = antPos & getNextPos ant.aAngle 20
    drawLineEx antPos antDir 5 color


trainingModeToString :: TrainingMode -> String
trainingModeToString tm = case tm of
    Off -> "Off"
    Slow -> "Slow"
    Fast -> "Fast"


renderAntAIWorld :: World -> IO ()
renderAntAIWorld w = do
    drawText ("Pheromones: " ++ show (Seq.length w.wPheromones)) 10 50 20 darkBrown
    drawText ("Training: " ++ trainingModeToString w.wTrainingMode) 10 75 20 darkBrown
    drawText ("Ticks: " ++ show w.wTicks) 10 100 20 darkBrown
    drawText ("Generation: " ++ show w.wGeneration) 10 125 20 darkBrown
    forM_ w.wAnts (drawAnt black)


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
                -- drawFPS 10 10
            }


driveAntAI :: IO ()
driveAntAI = initAntAIWorld >>= gameLoop antAISysWrapped windowShouldClose
