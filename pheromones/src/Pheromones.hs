{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use forM_" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-# HLINT ignore "Use <$>" #-}

module Pheromones where

import AntMovement (antMovementSys)
import Constants (bgColor, collisionRectSize, fps, initPheromoneAmount, maxPheromones, pheromoneColor, pheromoneScale, regeneratePheromoneDelay, screenHeight, screenWidth)
import Control.Monad (forM_, when)
import Data.Function ((&))
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
import Raylib.Util (drawing)
import Raylib.Util.Colors (lightGray)
import Shared (
    System (..),
    calcCenteredRect,
    calcRectCenter,
    defaultWorld,
    gameLoop,
    isPointInRect,
    mapAccumL',
 )
import System.Random (newStdGen)
import Types (
    Ant (..),
    Container (..),
    Food (..),
    Nest (..),
    Pheromone (..),
    World (..),
 )


initPheromoneWorld :: IO World
initPheromoneWorld = do
    _ <- initWindow screenWidth screenHeight "Pheromones"
    setTargetFPS fps
    setTraceLogLevel LogWarning
    setMouseCursor MouseCursorCrosshair
    rng <- newStdGen
    return (defaultWorld rng)


handlePheromoneInput :: World -> IO World
handlePheromoneInput w = return w


-- This function is meant to be with mapAccumL'
antDropsPheromone :: Nest -> Seq Food -> Seq Pheromone -> Ant -> (Ant, Seq Pheromone)
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
        (ant', !pheromones') =
            if hasFood
                && notOnFood
                && notOnPheromone
                && notOnNest
                && regenCounterGreaterThanDelay
                && underMaxPheromones
                then
                    let pheromoneRect = calcCenteredRect antPos collisionRectSize
                        pheromone = Pheromone (Container initPheromoneAmount pheromoneRect)
                    in  (ant{aRegeneratePheromoneCounter = 0}, pheromone <| pheromones)
                else (ant{aRegeneratePheromoneCounter = regenerationCounter + 1}, pheromones)
    in  (ant', pheromones')


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
        (playerAnt', pheromones'') = w.wPlayerAnt & antDropsPheromone w.wNest w.wFood pheromones'
        (ants', pheromones''') = w.wAnts & mapAccumL' (antDropsPheromone w.wNest w.wFood) pheromones''
    in  w{wPlayerAnt = playerAnt', wAnts = ants', wPheromones = pheromones'''}


-- Use a constant rectangle size for a pheromone, and just scale the amount circle.
drawPheromone :: Pheromone -> IO ()
drawPheromone (Pheromone (Container amount rect)) = do
    -- Draw pheromone as a circle
    let radius = int2Float amount * pheromoneScale
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
                clearBackground bgColor
                allSystems.render w
                -- drawFPS 10 10
            }


drivePheromones :: IO ()
drivePheromones =
    initPheromoneWorld >>= gameLoop pheromoneSysWrapped windowShouldClose