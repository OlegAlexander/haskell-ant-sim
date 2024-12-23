{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use forM_" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Pheromones where

import AntMovement (antMovementSys)
import Constants (
    antAcceleration,
    antMaxSpeed,
    antTurnAngle,
    collisionRectSize,
    foodColor,
    foodGrowthAmount,
    foodScale,
    fps,
    initPheromoneAmount,
    nestSize,
    pheromoneColor,
    pheromoneScale,
    regeneratePheromoneDelay,
    screenHeight,
    screenWidth,
 )
import Control.Monad (forM_, when)
import Data.Fixed (mod')
import Data.Foldable (find)
import Data.Function ((&))
import Data.IntMap (mapAccum)
import Data.List (findIndex, mapAccumL)
import Data.Maybe (mapMaybe)
import Data.Traversable (for)
import Debug.Trace (traceShowId)
import Food (foodSys)
import GHC.Float (int2Float)
import Raylib.Core (
    clearBackground,
    getMousePosition,
    initWindow,
    isKeyDown,
    isKeyPressed,
    isMouseButtonDown,
    setMouseCursor,
    setTargetFPS,
    setTraceLogLevel,
    toggleFullscreen,
    windowShouldClose,
 )
import Raylib.Core.Shapes (
    drawCircleV,
    drawLineEx,
    drawRectangleLinesEx,
    drawRectangleRec,
 )
import Raylib.Core.Text (drawFPS)
import Raylib.Core.Textures (loadTexture)
import Raylib.Types (
    KeyboardKey (..),
    MouseButton (MouseButtonLeft),
    MouseCursor (MouseCursorCrosshair),
    Rectangle (Rectangle),
    TraceLogLevel (LogWarning),
 )
import Raylib.Types.Core (MouseButton (MouseButtonRight), Vector2 (..))
import Raylib.Util (drawing)
import Raylib.Util.Colors (black, blue, brown, green, lightGray, red)
import Raylib.Util.Math ((|+|))
import Shared (
    System (..),
    calcCenteredRect,
    calcRectCenter,
    gameLoop,
    getNextPos,
    isPointInRect,
    mkAnt,
    setAt,
 )
import System.Random (mkStdGen, randomIO)
import Types (
    Ant (..),
    Container (..),
    EntityType (..),
    Food (..),
    GoDir (..),
    Nest (..),
    Pheromone (..),
    Sprite (..),
    WheelPos (..),
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
        walls = []
    -- pheromones =
    --     [ Pheromone
    --         ( Container
    --             initPheromoneAmount
    --             (calcCenteredRect (antPos |+| Vector2 100 100) collisionRectSize)
    --         )
    --     ]
    return $ World playerAnt [] nest True True False True walls Nothing [] Nothing []


handlePheromoneInput :: World -> IO World
handlePheromoneInput w = return w


-- This function is meant to be with mapAccumL
antDropsPheromone :: Nest -> [Food] -> [Pheromone] -> Ant -> ([Pheromone], Ant)
antDropsPheromone nest foods pheromones ant =
    -- Drop a pheromone if the ant has food,
    -- and it's not on top of a food,
    -- and it's not on top of a pheromone,
    -- and it's not on top of the nest
    -- and the ant's regeneration counter is greater than the regeneration delay.
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
        -- Force pheromones' because mapAccumL is lazy in the accumulator
        (!pheromones', ant') =
            if hasFood
                && notOnFood
                && notOnPheromone
                && notOnNest
                && regenCounterGreaterThanDelay
                then
                    let pheromoneRect = calcCenteredRect antPos collisionRectSize
                        pheromone = Pheromone (Container initPheromoneAmount pheromoneRect)
                    in  (pheromone : pheromones, ant{aRegeneratePheromoneCounter = 0})
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
                & map decrementPheromoneAmount
                & filter (\pheromone -> pheromone.pContainer.cAmount > 0)
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