{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use guards" #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}

module AntAI where

import AntMovement (antMovementSys, getCollisionRects, updateAntMovement)
import Constants (
    antVisionResolution,
    bgColor,
    compassMaxDistance,
    coursesPerGeneration,
    foodColor,
    inForagingBrainFile,
    inReturningBrainFile,
    outForagingBrainFile,
    outReturningBrainFile,
    fps,
    hitboxSize,
    maxGenerations,
    minWallSize,
    mutationRate,
    nnParameterRange,
    numAnts,
    screenHeight,
    screenWidth,
    ticksPerCourse, 
    fenceWallThickness,
    usePretrainedBrains
 )
import Control.Monad (forM_, replicateM, when)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Traversable (for)
import Data.Tuple (swap)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Debug.Pretty.Simple (pTraceShowId, pTraceShowM)
import Debug.Trace (traceShowId)
import DrawWalls (drawWallsSys)
import FlatlandRenderer (flatlandRendererSys, updateAntFR)
import Food (foodSys)
import GHC.Float (int2Float)
import NeuralNetwork (
    Layer,
    crossover,
    flattenLayers,
    forwardAll,
    invert,
    mutate,
    readFlatLayers,
    sigmoid,
    unflattenLayers,
    uniformListR,
    writeFlatLayers, FlatLayers,
 )
import Pheromones (pheromoneSys)
import Raylib.Core (
    clearBackground,
    getFPS,
    initWindow,
    isKeyDown,
    isKeyPressed,
    setExitKey,
    setMouseCursor,
    setTargetFPS,
    setTraceLogLevel,
    toggleFullscreen,
    windowShouldClose,
 )
import Raylib.Core.Shapes (drawCircleV, drawLineEx)
import Raylib.Core.Text (drawFPS, drawText)
import Raylib.Types (
    Color,
    KeyboardKey (..),
    MouseCursor (MouseCursorCrosshair),
    Rectangle (Rectangle),
    TraceLogLevel (LogWarning),
 )
import Raylib.Types.Core (Vector2 (..))
import Raylib.Util (drawing)
import Raylib.Util.Colors (black, blue, darkBrown, green, lightGray)
import Shared (
    System (..),
    calcRectCenter,
    defaultWorld,
    drawTextLines',
    gameLoop,
    getNextPos,
    isPointInRect,
    mapAccumL',
    mkAnt,
    normalize,
    rgbToLinear,
    fenceWalls,
 )
import System.Mem (performGC)
import System.Random (StdGen, newStdGen, random, randomIO, randomR)
import Text.Printf (printf)
import Types (
    Ant (..),
    AntDecision (..),
    Container (..),
    Food (..),
    GoDir (..),
    Nest (..),
    Sprite (..),
    TrainingMode (..),
    VisionRay (..),
    WheelPos (Center, TurnLeft, TurnRight),
    World (..),
 )


-- Generate 1 random wall with a random size and position.
-- The wall should be within the screen bounds.
-- The wall should not be smaller than minWallSize.
-- The wall should not overlap the nest.
generateRandomWall :: StdGen -> Nest -> (Rectangle, StdGen)
generateRandomWall rng nest =
    let (!x, !rng') = randomR (0, int2Float screenWidth) rng
        (!y, !rng'') = randomR (0, int2Float screenHeight) rng'
        (!w, !rng''') = randomR (minWallSize, int2Float screenWidth - x) rng''
        (!h, !rng'''') = randomR (minWallSize, int2Float screenHeight - y) rng'''
        wall = Rectangle x y w h
        nestCenter = calcRectCenter nest.nContainer.cRect
    in  if isPointInRect nestCenter wall
            then generateRandomWall rng'''' nest
            else (wall, rng'''')


generateRandomWalls :: StdGen -> Nest -> Int -> (Seq Rectangle, StdGen)
generateRandomWalls rng nest numWalls =
    let (!walls, !rng') = mapAccumL' (\rgen _ -> generateRandomWall rgen nest) rng [1 .. numWalls]
    in  (fenceWalls fenceWallThickness <> Seq.fromList walls, rng')


-- Generate 1 random food object with a random position and food amount.
-- Food should be within the screen bounds.
-- Food should not overlap the nest or the walls.
generateRandomFood :: StdGen -> Nest -> Seq Rectangle -> (Food, StdGen)
generateRandomFood rng nest walls =
    let (!x, !rng') = randomR (0, int2Float screenWidth) rng
        (!y, !rng'') = randomR (0, int2Float screenHeight) rng'
        (Vector2 !nestX !nestY) = nest.nContainer.cRect & calcRectCenter
        dx = nestX - x
        dy = nestY - y
        distance = normalize (sqrt (dx * dx + dy * dy)) compassMaxDistance
        -- amount = ceiling (150 * (distance ** 3))
        amount = ceiling (1_000_000 * (distance ** 3))
        food = Food (Container amount (Rectangle x y hitboxSize hitboxSize))
        foodOverlapsNest = isPointInRect (Vector2 x y) nest.nContainer.cRect
        foodOverlapsWalls = any (isPointInRect (Vector2 x y)) walls
    in  if foodOverlapsNest || foodOverlapsWalls
            then generateRandomFood rng'' nest walls
            else (food, rng'')


generateRandomFoods :: StdGen -> Nest -> Seq Rectangle -> Int -> (Seq Food, StdGen)
generateRandomFoods rng nest walls numFoods =
    let (!foods, !rng') = mapAccumL' (\rgen _ -> generateRandomFood rgen nest walls) rng [1 .. numFoods]
    in  (Seq.fromList foods, rng')


-- Make a new ant but keep the old ant's brains and score
resetAnt :: StdGen -> Ant -> (Ant, StdGen)
resetAnt rng ant =
    let screenCenter = Vector2 (int2Float screenWidth / 2) (int2Float screenHeight / 2)
        (!ant', !rng') = mkAnt rng screenCenter
        ant'' =
            ant'
                { aForagingBrain = ant.aForagingBrain,
                  aReturningBrain = ant.aReturningBrain,
                  aScore = ant.aScore
                }
    in  (ant'', rng')


-- Same as resetAnt but also reset the score to 0
hardResetAnt :: StdGen -> Ant -> (Ant, StdGen)
hardResetAnt gen ant =
    let (!ant', !rng') = resetAnt gen ant
    in  (ant'{aScore = 0}, rng')


resetAllAnts :: StdGen -> Seq Ant -> (Seq Ant, StdGen)
resetAllAnts rng ants = mapAccumL' resetAnt rng ants


hardResetAllAnts :: StdGen -> Seq Ant -> (Seq Ant, StdGen)
hardResetAllAnts rng ants = mapAccumL' hardResetAnt rng ants


getParents :: StdGen -> Seq a -> (a, a, StdGen)
getParents rng xs =
    let (!parent1Index, !rng') = randomR (0, Seq.length xs - 1) rng
        (!parent2Index, !rng'') = randomR (0, Seq.length xs - 1) rng'
        parent1 = xs `Seq.index` parent1Index
        parent2 = xs `Seq.index` parent2Index
    in  (parent1, parent2, rng'')


generateNewBrain :: StdGen -> [Layer] -> [Layer] -> ([Layer], StdGen)
generateNewBrain rng p1Brain p2Brain =
    let (!newBrain, !rng') =
            crossover (flattenLayers p1Brain) (flattenLayers p2Brain) rng
                & mutate mutationRate 0.5
                & invert (mutationRate * 0.001)
    in  (unflattenLayers newBrain, rng')


generateNewAnt :: StdGen -> Seq Ant -> (Ant, StdGen)
generateNewAnt rng ants =
    let screenCenter = Vector2 (int2Float screenWidth / 2) (int2Float screenHeight / 2)
        (!p1, !p2, !rng') = getParents rng ants
        (!foragingBrain, !rng'') = generateNewBrain rng' p1.aForagingBrain p2.aForagingBrain
        (!returningBrain, !rng''') = generateNewBrain rng'' p1.aReturningBrain p2.aReturningBrain
        (!newAnt, !rng'''') = mkAnt rng''' screenCenter
    in  (newAnt{aForagingBrain = foragingBrain, aReturningBrain = returningBrain}, rng'''')


average :: (Fractional a) => [a] -> a
average xs = if not (null xs) then sum xs / fromIntegral (length xs) else 0


stdDev :: (Floating a) => [a] -> a
stdDev xs =
    if not (null xs)
        then
            let avg = average xs
                variance = sum (fmap (\x -> (x - avg) ** 2) xs) / fromIntegral (length xs)
            in  sqrt variance
        else 0


-- TODO Move these selection functions to a separate module
-- Sort by score and keep the top n%.
truncationSelection :: (a -> Float) -> Float -> Seq a -> Seq a
truncationSelection getScore percentage xs =
    let numToKeep = round (fromIntegral (Seq.length xs) * (percentage / 100))
    in  xs & sortByScore getScore & Seq.take numToKeep


-- Sort by score from highest to lowest
sortByScore :: (a -> Float) -> Seq a -> Seq a
sortByScore getScore xs = Seq.sortBy (\a b -> compare (getScore b) (getScore a)) xs


-- Choose k random members and return the winner.
tournament :: (a -> Float) -> Int -> StdGen -> Seq a -> (a, StdGen)
tournament getScore k rng xs =
    let (!indices, !rng') = uniformListR k (0, Seq.length xs - 1) rng
        contenders = Seq.fromList (fmap (Seq.index xs) indices)
        winner = sortByScore getScore contenders `Seq.index` 0
    in  (winner, rng')


-- Run n tournaments and return the winner of each.
tournamentSelection :: (a -> Float) -> Int -> Int -> StdGen -> Seq a -> (Seq a, StdGen)
tournamentSelection getScore n k rng xs =
    let (!winners, !rng') = mapAccumL' (\rgen _ -> tournament getScore k rgen xs) rng [1 .. n]
    in  (Seq.fromList winners, rng')


-- Use truncation selection to choose the parents
-- For each ant in numAnts, generate a new ant brain by crossing over
-- the brains of 2 random ants and then mutate it.
-- Maybe keep the best ant brains from the previous generation (Elitism).
-- Return the new generation of ants.
generateNextGeneration' :: Int -> StdGen -> Seq Ant -> (Seq Ant, Int, StdGen)
generateNextGeneration' generation rng ants =
    let selectedAnts = truncationSelection (.aScore) 33 ants
        (!topAnts, !rng') = selectedAnts & hardResetAllAnts rng
        (!newAnts, !rng'') = mapAccumL' (\rgen _ -> generateNewAnt rgen topAnts) rng' [1 .. (numAnts - Seq.length topAnts)]
        _ = traceShowId ("generation", generation + 1)
    in  (topAnts <> Seq.fromList newAnts, generation + 1, rng'')


-- Use tournament selection to choose the parents
-- For each ant in numAnts, generate a new ant brain by crossing over
-- the brains of 2 random ants and then mutate it.
-- Maybe keep the best ant brains from the previous generation (Elitism).
-- Return the new generation of ants.
generateNextGeneration :: Int -> StdGen -> Seq Ant -> (Seq Ant, Int, StdGen)
generateNextGeneration generation rng ants =
    let (!eliteAnts, !rng') = ants & sortByScore (.aScore) & Seq.take 5 & hardResetAllAnts rng
        (!selectedAnts, !rng'') = tournamentSelection (.aScore) 100 3 rng' ants
        (!parents, !rng''') = hardResetAllAnts rng'' selectedAnts
        numChildren = numAnts - Seq.length eliteAnts
        (!children, !rng'''') = mapAccumL' (\rgen _ -> generateNewAnt rgen parents) rng''' [1 .. numChildren]
        _ = traceShowId ("generation", generation + 1)
    in  (eliteAnts <> Seq.fromList children, generation + 1, rng'''')


loadAntBrains :: FlatLayers -> FlatLayers -> Ant -> Ant
loadAntBrains foragingBrain returningBrain ant =
    ant
        { aForagingBrain = unflattenLayers foragingBrain,
          aReturningBrain = unflattenLayers returningBrain
        }


initAntAIWorld :: IO World
initAntAIWorld = do
    rng <- newStdGen
    let screenCenter = Vector2 (int2Float screenWidth / 2) (int2Float screenHeight / 2)
        (!ants, !rng') = mapAccumL' mkAnt rng (replicate numAnts screenCenter)
    _ <- initWindow screenWidth screenHeight "Ant AI"
    setTargetFPS fps
    setTraceLogLevel LogWarning
    -- setMouseCursor MouseCursorCrosshair
    setExitKey KeyNull -- Pressing ESC should not exit the game
    ants' <- if usePretrainedBrains 
        then do
            -- Load the flat layers from the files
            foraging <- readFlatLayers inForagingBrainFile
            returning <- readFlatLayers inReturningBrainFile
            return $ map (loadAntBrains foraging returning) ants
        else return ants
    return (defaultWorld rng'){wAnts = Seq.fromList ants'}


handleAntAIInput :: World -> IO World
handleAntAIInput w = do
    tKey <- isKeyPressed KeyT
    leftShiftKey <- isKeyDown KeyLeftShift
    rightShiftKey <- isKeyDown KeyRightShift
    let shiftTPressed = tKey && (leftShiftKey || rightShiftKey)
        trainingMode = case (tKey, shiftTPressed, w.wTrainingMode) of
            (True, False, Off) -> Fast
            (_, True, Fast) -> Slow
            (_, True, Slow) -> Fast
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
    -- TODO Add speed?
    let visionRayColors =
            ant.aVisionRays
                & concatMap (\ray -> let (r, g, b, a) = rgbToLinear ray.rColor in [r, g, b])
        nestAngle = ant.aNestAngle
        nestAntAngleDelta = ant.aNestAntAngleDelta
        antNestDistance = ant.aNestDistance
        inputVector = visionRayColors ++ [nestAngle, nestAntAngleDelta, antNestDistance]
    in  -- 32 * 3 + 3 = 99 inputs
        V.fromList inputVector


applyAntDecision :: AntDecision -> Ant -> Ant
applyAntDecision decision ant = case decision of
    GoNowhere -> ant{aWheelPos = Center, aGoDir = Stop}
    GoLeft -> ant{aWheelPos = TurnLeft, aGoDir = Stop}
    GoForwardLeft -> ant{aWheelPos = TurnLeft, aGoDir = Forward}
    GoForward -> ant{aWheelPos = Center, aGoDir = Forward}
    GoForwardRight -> ant{aWheelPos = TurnRight, aGoDir = Forward}
    GoRight -> ant{aWheelPos = TurnRight, aGoDir = Stop}


-- GoBackwardRight -> ant{aWheelPos = TurnRight, aGoDir = Backward}
-- GoBackward -> ant{aWheelPos = Center, aGoDir = Backward}
-- GoBackwardLeft -> ant{aWheelPos = TurnLeft, aGoDir = Backward}

-- If the training mode is Off or Done, return the current ticks and courses
-- If the training mode is Slow or Fast, increment ticks by 1.
-- If ticks is equal to ticksPerCourse, reset ticks to 0 and increment courses by 1.
-- If courses is equal to coursesPerGeneration, reset courses to 0.
calcTicksAndCourse :: World -> (Int, Int)
calcTicksAndCourse w =
    let (!ticks, !course) = case w.wTrainingMode of
            Off -> (w.wTicks, w.wCourse)
            Slow -> (w.wTicks + 1, w.wCourse)
            Fast -> (w.wTicks + 1, w.wCourse)
            Done -> (w.wTicks, w.wCourse)
        (!ticks', !course') =
            if ticks == ticksPerCourse
                then (0, course + 1)
                else
                    if course == coursesPerGeneration
                        then (0, 0)
                        else (ticks, course)
    in  (ticks', course')


getAntDecision :: Ant -> AntDecision
getAntDecision ant =
    let brain = if ant.aHasFood then ant.aReturningBrain else ant.aForagingBrain
    in  ant & mkInputVector & antBrainNeuralNetwork brain


-- TODO Clean up this function!
updateAntAIWorld :: World -> World
updateAntAIWorld w =
    let antDecisions = w.wAnts & fmap getAntDecision
        decidedAnts = w.wAnts & Seq.zipWith applyAntDecision antDecisions
        collisionRects = getCollisionRects w
        (!movedAnts, !rng) = mapAccumL' (updateAntMovement collisionRects) w.wRng decidedAnts
        seeingAnts = fmap (updateAntFR w) movedAnts
        (!walls, !rng') = if (w.wTrainingMode == Slow || w.wTrainingMode == Fast) && w.wTicks == 0 then generateRandomWalls rng w.wNest 3 else (w.wWalls, rng)
        (!foods, !rng'') = if (w.wTrainingMode == Slow || w.wTrainingMode == Fast) && w.wTicks == 0 then generateRandomFoods rng' w.wNest walls 1 else (w.wFood, rng')
        (!resetAnts, !rng''') = if (w.wTrainingMode == Slow || w.wTrainingMode == Fast) && w.wTicks == 0 then resetAllAnts rng'' seeingAnts else (seeingAnts, rng'')
        avgScore = if (w.wTrainingMode == Slow || w.wTrainingMode == Fast) && w.wTicks == 0 && w.wCourse == 0 then average (fmap (.aScore) (toList resetAnts)) else w.wBestAvgScore
        (!newAnts, !rng'''') = if (w.wTrainingMode == Slow || w.wTrainingMode == Fast) && w.wTicks == 0 && w.wCourse == 0 then hardResetAllAnts rng''' resetAnts else (resetAnts, rng''')
        (!newAnts', !generation', !rng''''') = if (w.wTrainingMode == Slow || w.wTrainingMode == Fast) && w.wTicks == 0 && w.wCourse == 0 then generateNextGeneration w.wGeneration rng'''' resetAnts else (newAnts, w.wGeneration, rng'''')
        _ = if (w.wTrainingMode == Slow || w.wTrainingMode == Fast) && w.wTicks == 0 && w.wCourse == 0 then traceShowId ("avgScore", avgScore, w.wBestAvgScore) else ("", 0, 0)
        pheromones = if (w.wTrainingMode == Slow || w.wTrainingMode == Fast) && w.wTicks == 0 then Seq.empty else w.wPheromones
        (!ticks, !course) = calcTicksAndCourse w
        trainingMode = if generation' == maxGenerations then Done else w.wTrainingMode
    in  w
            { wAnts = newAnts',
              wTicks = ticks,
              wCourse = course,
              wGeneration = generation',
              wTrainingMode = trainingMode,
              wBestAvgScore = max avgScore w.wBestAvgScore,
              wWalls = walls,
              wFood = foods,
              wPheromones = pheromones,
              wRng = rng'''''
            }


-- TODO Move this to Shared
-- TODO Draw ants on top of all other objects
drawAnt :: Color -> Ant -> IO ()
drawAnt color ant = do
    let antPos = ant.aPos
    -- If the ant has food, draw a piece of food in its mouth
    when ant.aHasFood $ do
        let foodPiecePos = getNextPos ant.aAngle 20 antPos
        drawCircleV foodPiecePos 8 foodColor
    -- Draw the ant
    drawCircleV antPos 5 color
    let antDir = antPos & getNextPos ant.aAngle 20
    drawLineEx antPos antDir 5 color


writeBestBrain :: World -> IO ()
writeBestBrain w = do
    let bestAnt = sortByScore (.aScore) w.wAnts `Seq.index` 0
    writeFlatLayers outForagingBrainFile (flattenLayers bestAnt.aForagingBrain)
    writeFlatLayers outReturningBrainFile (flattenLayers bestAnt.aReturningBrain)


renderAntAIWorld :: World -> IO ()
renderAntAIWorld w = do
    if w.wTrainingMode == Fast
        then do setTargetFPS 1000
        else setTargetFPS fps
    gameFps <- getFPS
    drawTextLines'
        [ "FPS: " ++ show gameFps,
          "Pheromones: " ++ show (Seq.length w.wPheromones),
          "Ants: " ++ show (Seq.length w.wAnts),
          "Training: " ++ show w.wTrainingMode,
          "Generation: " ++ show (w.wGeneration + 1),
          "Course: " ++ show (w.wCourse + 1),
          "Ticks: " ++ show (w.wTicks + 1),
          "Best Avg Score: " ++ show w.wBestAvgScore
        ]
    forM_ w.wAnts (drawAnt black)
    -- Draw the score above the ants
    -- forM_ w.wAnts $ \ant -> do
    --     let (Vector2 x y) = ant.aPos
    --         antScore = ant.aScore
    --         scoreText = printf "%.2f" antScore
    --         (Vector2 tx ty) = Vector2 (x - 20) (y - 40)
    --     when (ant.aScore > 0) $ drawText scoreText (floor tx) (floor ty) 30 blue
    when ((w.wTrainingMode == Slow || w.wTrainingMode == Fast) && w.wTicks == 0) performGC
    when ((w.wTrainingMode == Slow || w.wTrainingMode == Fast) && w.wTicks == 0 && w.wCourse == 0) (writeBestBrain w)


updateAntAIWorldDebug :: World -> World
updateAntAIWorldDebug w =
    let w' = pTraceShowId w
            & updateAntAIWorld
            & pTraceShowId
    in w'

antAISys :: System World
antAISys = System handleAntAIInput updateAntAIWorld renderAntAIWorld


antAISysWrapped :: System World
antAISysWrapped =
    let allSystems =
            drawWallsSys
                <> pheromoneSys
                <> foodSys
                <> antMovementSys
                <> flatlandRendererSys
                <> antAISys
    in  allSystems
            { render = \w -> drawing $ do
                f11Pressed <- isKeyPressed KeyF11
                when f11Pressed toggleFullscreen
                clearBackground bgColor
                allSystems.render w
            }


driveAntAI :: IO ()
driveAntAI = initAntAIWorld >>= gameLoop antAISysWrapped windowShouldClose
