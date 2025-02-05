{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use forM_" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Pheromones where

import AntMovement (antMovementSys)
import Constants (
    collisionRectSize,
    fps,
    initPheromoneAmount,
    maxPheromones,
    pheromoneColor,
    pheromoneScale,
    regeneratePheromoneDelay,
    screenHeight,
    screenWidth,
 )
import Control.Monad (forM_, when)
import Data.Function ((&))
import Data.List (mapAccumL)
import Data.Sequence (Seq, (<|))
import Data.Sequence qualified as Seq
import Food (foodSys)
import GHC.Float (int2Float)
import Raylib.Core (
    clearBackground,
    initWindow,
    isKeyPressed,
    setMouseCursor,
    setTargetFPS,
    setTraceLogLevel,
    toggleFullscreen,
    windowShouldClose,
 )
import Raylib.Core.Shapes (drawCircleV)
import Raylib.Types (
    KeyboardKey (..),
    MouseCursor (MouseCursorCrosshair),
    TraceLogLevel (LogWarning),
 )
import Raylib.Types.Core (Vector2 (..))
import Raylib.Util (drawing)
import Raylib.Util.Colors (lightGray)
import Shared (
    System (..),
    calcCenteredRect,
    calcRectCenter,
    gameLoop,
    isPointInRect,
    mkAnt,
 )
import System.Random (randomIO)
import Types (
    Ant (..),
    Container (..),
    Food (..),
    Nest (..),
    Pheromone (..),
    TrainingMode (..),
    World (..),
 )


initPheromoneWorld :: IO World
initPheromoneWorld = do
    _ <- initWindow screenWidth screenHeight "Pheromones"
    setTargetFPS fps
    setTraceLogLevel LogWarning
    setMouseCursor MouseCursorCrosshair
    seed <- randomIO
    let antPos = Vector2 (int2Float screenWidth / 2) (int2Float screenHeight / 2)
        playerAnt = mkAnt antPos seed
        nest = Nest (Container 0 (calcCenteredRect antPos collisionRectSize))
    -- pheromones =
    --     [ Pheromone
    --         ( Container
    --             initPheromoneAmount
    --             (calcCenteredRect (antPos |+| Vector2 100 100) collisionRectSize)
    --         )
    --     ]
    return $ World playerAnt Seq.empty nest True True False True Seq.empty Nothing Seq.empty Nothing Seq.empty Off 0 0


handlePheromoneInput :: World -> IO World
handlePheromoneInput w = return w


-- This function is meant to be with mapAccumL
antDropsPheromone :: Nest -> Seq Food -> Seq Pheromone -> Ant -> (Seq Pheromone, Ant)
antDropsPheromone nest foods pheromones ant =
    -- Drop a pheromone if the ant has food,
    -- and it's not on top of a food,
    -- and it's not on top of a pheromone (this may be too slow),
    -- and it's not on top of the nest
    -- and the ant's regeneration counter is greater than the regeneration delay.
    -- and the total number of pheromones is less than maxPheromones. Sadly, this is needed for efficiency.
    -- Otherwise, just increment the regeneration counter.
    -- TODO: The increment counter logic should be a little more complicated.
    let antPos = ant.aPos
        hasFood = ant.aHasFood
        notOnFood =
            not (any (\food -> isPointInRect antPos food.fContainer.cRect) foods)
        notOnPheromone =
            not (any (\pheromone -> isPointInRect antPos pheromone.pContainer.cRect) pheromones)
        notOnNest =
            not (isPointInRect antPos nest.nContainer.cRect)
        regenerationCounter = ant.aRegeneratePheromoneCounter
        regenCounterGreaterThanDelay = regenerationCounter > regeneratePheromoneDelay
        underMaxPheromones = length pheromones < maxPheromones
        -- Force pheromones' because mapAccumL is lazy in the accumulator
        (!pheromones', ant') =
            if hasFood
                && notOnFood
                && notOnPheromone
                && notOnNest
                && regenCounterGreaterThanDelay
                && underMaxPheromones
                then
                    let pheromoneRect = calcCenteredRect antPos collisionRectSize
                        pheromone = Pheromone (Container initPheromoneAmount pheromoneRect)
                    in  (pheromone <| pheromones, ant{aRegeneratePheromoneCounter = 0})
                else (pheromones, ant{aRegeneratePheromoneCounter = regenerationCounter + 1})
    in  (pheromones', ant')


decrementPheromoneAmount :: Pheromone -> Pheromone
decrementPheromoneAmount (Pheromone (Container amount rect)) =
    Pheromone (Container (amount - 1) rect)


updatePheromoneWorld :: World -> World
updatePheromoneWorld w =
    -- Evaporate pheromones until they disappear
    let pheromones' =
            w.wPheromones
                -- TODO Fuse this map and filter
                & fmap decrementPheromoneAmount
                & Seq.filter (\pheromone -> pheromone.pContainer.cAmount > 0)
        (pheromones'', playerAnt') = w.wPlayerAnt & antDropsPheromone w.wNest w.wFood pheromones'
        (pheromones''', ants') = w.wAnts & mapAccumL (antDropsPheromone w.wNest w.wFood) pheromones''
    in  w{wPlayerAnt = playerAnt', wAnts = ants', wPheromones = pheromones'''}


-- Use a constant rectangle size for a pheromone, and just scale the amount circle.
drawPheromone :: Pheromone -> IO ()
drawPheromone (Pheromone (Container amount rect)) = do
    -- Draw pheromone as a circle
    let radius = int2Float amount * pheromoneScale + 3
        pos = calcRectCenter rect
    drawCircleV pos radius pheromoneColor

    -- Draw the collision rectangle
    -- drawRectangleLinesEx rect 2 black
    return () -- Deal with the formatter :(


renderPheromoneWorld :: World -> IO ()
renderPheromoneWorld w = forM_ w.wPheromones drawPheromone


pheromoneSys :: System World
pheromoneSys = System handlePheromoneInput updatePheromoneWorld renderPheromoneWorld


pheromoneSysWrapped :: System World
pheromoneSysWrapped =
    let allSystems = pheromoneSys <> foodSys <> antMovementSys
    in  allSystems
            { render = \w -> drawing $ do
                f11Pressed <- isKeyPressed KeyF11
                when f11Pressed toggleFullscreen
                clearBackground lightGray
                allSystems.render w
                -- drawFPS 10 10
            }


drivePheromones :: IO ()
drivePheromones =
    initPheromoneWorld >>= gameLoop pheromoneSysWrapped windowShouldClose