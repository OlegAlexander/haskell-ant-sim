{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use forM_" #-}
{-# HLINT ignore "Avoid lambda" #-}

module Pheromones where

import AntMovement (antMovementSys)
import Constants (antAcceleration, antMaxSpeed, antPng, antTurnAngle, collisionRectSize, foodColor, foodGrowthAmount, foodScale, fps, initPheromoneAmount, nestSize, pheromoneColor, pheromoneScale, screenHeight, screenWidth)
import Control.Monad (forM_, when)
import Data.Fixed (mod')
import Data.Foldable (find)
import Data.Function ((&))
import Data.List (findIndex)
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
    setAt,
 )
import System.Random (mkStdGen)
import Types (
    Ant (..),
    Container (..),
    EntityType (..),
    Food (..),
    GoDir (..),
    Mode (..),
    Nest (..),
    Pheromone (Pheromone),
    Sprite (..),
    WheelPos (..),
    World (..),
 )


initPheromoneWorld :: IO World
initPheromoneWorld = do
    let screenCenterW = int2Float screenWidth / 2
        screenCenterH = int2Float screenHeight / 2
        walls = []
    _ <- initWindow screenWidth screenHeight "Pheromones"
    setTargetFPS fps
    setTraceLogLevel LogWarning
    setMouseCursor MouseCursorCrosshair
    let rng = mkStdGen 0
        antPos = Vector2 screenCenterW screenCenterH
        nest = Nest (Container 0 (calcCenteredRect antPos collisionRectSize))
        playerAnt = Ant antPos 0 0 SeekFood rng Stop Center LeftSprite [] 0 0 False 0
        pheromones = [Pheromone (Container initPheromoneAmount (calcCenteredRect (antPos |+| Vector2 100 100) collisionRectSize))]
    return $ World playerAnt nest True True False True walls Nothing [] Nothing pheromones


handlePheromoneInput :: World -> IO World
handlePheromoneInput w = return w


updatePheromoneWorld :: World -> World
updatePheromoneWorld w =
    -- Dry up pheromones until they disappear
    let pheromones = wPheromones w
        pheromones' = map (\(Pheromone (Container amount rect)) -> Pheromone (Container (amount - 1) rect)) pheromones
    in  w{wPheromones = filter (\(Pheromone (Container amount _)) -> amount > 0) pheromones'}


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
renderPheromoneWorld w = forM_ (wPheromones w) drawPheromone


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
                render allSystems w
                -- drawFPS 10 10
            }


drivePheromones :: IO ()
drivePheromones =
    initPheromoneWorld >>= gameLoop pheromoneSysWrapped windowShouldClose