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
    foodColor,
    fps,
    maxGenerations,
    minWallSize,
    numAnts,
    screenHeight,
    screenWidth,
    ticksPerGeneration,
 )
import Data.Sequence qualified as Seq
import Data.Traversable (for)
import Data.Tuple (swap)
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
    getFPS,
    initWindow,
    isKeyDown,
    isKeyPressed,
    setMouseCursor,
    setTargetFPS,
    toggleFullscreen,
    windowShouldClose,
 )
import Raylib.Core.Shapes (drawCircleV, drawLineEx)
import Raylib.Core.Text (drawText)
import Raylib.Types (
    Color,
    KeyboardKey (..),
    MouseCursor (MouseCursorCrosshair),
    Rectangle,
 )
import Raylib.Types.Core (Vector2 (..))
import Raylib.Util (drawing)
import Raylib.Util.Colors (black, darkBrown, lightGray)
import Shared (
    System (..),
    defaultWorld,
    drawTextLines',
    gameLoop,
    getNextPos,
    isPointInRect,
    mapAccumL',
    mkAnt,
    rgbToLinear,
 )
import System.Random (StdGen, newStdGen, random, randomIO)
import Types (
    Ant (..),
    AntDecision (..),
    Food,
    GoDir (..),
    Nest (..),
    TrainingMode (..),
    VisionRay (..),
    WheelPos (Center, TurnLeft, TurnRight),
    World (..),
 )


-- Generate n random walls with random positions and sizes.
-- The walls should be within the screen bounds.
-- The walls should not be smaller than minWallSize.
-- The walls should not overlap the nest.
generateRandomWalls :: StdGen -> Nest -> Int -> ([Rectangle], StdGen)
generateRandomWalls rng nest n = undefined


-- Generate n random food objects with random positions and food amounts.
-- Foods should be within the screen bounds.
-- Foods should not overlap the nest.
generateRandomFoods :: StdGen -> Int -> ([Food], StdGen)
generateRandomFoods rng n = undefined


generateRandomSeed :: StdGen -> Int -> (StdGen, Int)
generateRandomSeed rng _ = swap (random rng)


initAntAIWorld :: IO World
initAntAIWorld = do
    rng <- newStdGen
    let screenCenter = Vector2 (int2Float screenWidth / 2) (int2Float screenHeight / 2)
        (ants, rng') = mapAccumL' mkAnt rng (replicate numAnts screenCenter)
    _ <- initWindow screenWidth screenHeight "Ant AI"
    setTargetFPS fps
    setMouseCursor MouseCursorCrosshair
    return (defaultWorld rng'){wAnts = Seq.fromList ants}


handleAntAIInput :: World -> IO World
handleAntAIInput w = do
    tKey <- isKeyPressed KeyT
    leftShiftKey <- isKeyDown KeyLeftShift
    rightShiftKey <- isKeyDown KeyRightShift
    let shiftTPressed = tKey && (leftShiftKey || rightShiftKey)
        trainingMode = case (tKey, shiftTPressed, w.wTrainingMode) of
            (True, False, Off) -> Slow
            (_, True, Slow) -> Fast
            (_, True, Fast) -> Slow
            _ -> w.wTrainingMode
    return w{wTrainingMode = trainingMode}


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

-- If the training mode is Off or Done, return the current ticks and generation.
-- If the training mode is Slow or Fast, increment ticks by 1.
-- If ticks is equal to ticksPerGeneration, reset ticks to 0 and increment generation by 1.
calcTicksAndGeneration :: World -> (Int, Int)
calcTicksAndGeneration w =
    let (ticks, generation) = case w.wTrainingMode of
            Off -> (w.wTicks, w.wGeneration)
            Slow -> (w.wTicks + 1, w.wGeneration)
            Fast -> (w.wTicks + 1, w.wGeneration)
            Done -> (w.wTicks, w.wGeneration)
        (ticks', generation') =
            if ticks == ticksPerGeneration
                then (0, generation + 1)
                else (ticks, generation)
    in  (ticks', generation')


updateAntAIWorld :: World -> World
updateAntAIWorld w =
    let antDecisions =
            w.wAnts & fmap (\ant -> antBrainNeuralNetwork ant.aBrain (mkInputVector ant))
        (ants, w') =
            w.wAnts
                & Seq.zipWith applyAntDecision antDecisions
                & fmap (updateAntMovement w)
                & mapAccumL' updateAntFR w
        (ticks, generation) = calcTicksAndGeneration w'
        trainingMode = if generation == maxGenerations then Done else w'.wTrainingMode
    in  w'
            { wAnts = ants,
              wTicks = ticks,
              wGeneration = generation,
              wTrainingMode = trainingMode
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


renderAntAIWorld :: World -> IO ()
renderAntAIWorld w = do
    gameFps <- getFPS
    drawTextLines'
        [ "FPS: " ++ show gameFps,
          "Pheromones: " ++ show (Seq.length w.wPheromones),
          "Training: " ++ show w.wTrainingMode,
          "Generation: " ++ show w.wGeneration,
          "Ticks: " ++ show w.wTicks
        ]
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
