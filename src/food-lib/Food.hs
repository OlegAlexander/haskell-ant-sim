{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use forM_" #-}

module Food where

import AntMovement (antMovementSys)
import Constants (
    antAcceleration,
    antMaxSpeed,
    antPng,
    antTurnAngle,
    collisionRectSize,
    foodColor,
    foodGrowthAmount,
    foodScale,
    fps,
    screenHeight,
    screenWidth,
 )
import Control.Monad (forM_, when)
import Data.Fixed (mod')
import Data.Function ((&))
import Data.Maybe (mapMaybe)
import Debug.Trace (traceShowId)
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
import Shared (System (..), calcCenteredRect, gameLoop, getNextPos, isPointInRect)
import System.Random (mkStdGen)
import Types (
    Ant (..),
    EntityType (..),
    Food (..),
    GoDir (..),
    Mode (..),
    Sprite (..),
    WheelPos (..),
    World (..),
 )


initFoodWorld :: IO World
initFoodWorld = do
    let screenCenterW = int2Float screenWidth / 2
        screenCenterH = int2Float screenHeight / 2
        walls = []
    window <- initWindow screenWidth screenHeight "Food"
    setTargetFPS fps
    setTraceLogLevel LogWarning
    setMouseCursor MouseCursorCrosshair
    antTexture <- loadTexture antPng window
    let rng = mkStdGen 0
        antPos = Vector2 screenCenterW screenCenterH
        nestPos = antPos
        playerAnt = Ant antPos 0 0 SeekFood rng Stop Center LeftSprite [] 0 0 False
    return $ World window antTexture playerAnt nestPos True True False True walls Nothing [] Nothing


-- When the mouse is clicked, add a Food object at that position to foodBeingDrawn.
-- Continue incrementing foodAmount while the mouse is held down.
-- When the mouse is released, stop incrementing foodAmount and add the
--      foodBeingDrawn object to the food list.
-- Right click to remove food objects.
handleFoodInput :: World -> IO World
handleFoodInput w = do
    mousePos <- getMousePosition
    isMouseLeftPressed <- isMouseButtonDown MouseButtonLeft
    isMouseRightPressed <- isMouseButtonDown MouseButtonRight
    case (isMouseLeftPressed, isMouseRightPressed, wFoodBeingDrawn w) of
        (True, _, Nothing) ->
            let collisionRect = calcCenteredRect mousePos collisionRectSize
            in  return $ w{wFoodBeingDrawn = Just (Food mousePos foodGrowthAmount collisionRect)}
        (True, _, Just (Food pos amount rect)) ->
            return $ w{wFoodBeingDrawn = Just (Food pos (amount + foodGrowthAmount) rect)}
        (False, _, Just food) ->
            return $ w{wFoodBeingDrawn = Nothing, wFood = food : wFood w}
        (False, True, Nothing) ->
            let foodToKeep = filter (not . isPointInRect mousePos . foodCollisionRect) (wFood w)
            in  return w{wFood = foodToKeep}
        (False, False, Nothing) ->
            return w


updateFoodWorld :: World -> World
updateFoodWorld w = w


-- Use a constant rectangle size for food, and just scale the amount circle.
drawFood :: Food -> IO ()
drawFood (Food pos amount rect) = do
    -- Draw food as a circle
    let radius = int2Float amount * foodScale
    drawCircleV pos radius foodColor

    -- Draw the collision rectangle
    drawRectangleLinesEx rect 2 black


renderFoodWorld :: World -> IO ()
renderFoodWorld w = do
    -- Draw all food
    forM_ (wFood w) drawFood

    -- Draw foodBeingDrawn (Note to self: I can use forM_ here, too.)
    case wFoodBeingDrawn w of
        Just food -> drawFood food
        Nothing -> return ()


foodSys :: System World
foodSys = System handleFoodInput updateFoodWorld renderFoodWorld


foodSysWrapped :: System World
foodSysWrapped =
    let allSystems = foodSys <> antMovementSys
    in  allSystems
            { render = \w -> drawing $ do
                f11Pressed <- isKeyPressed KeyF11
                when f11Pressed toggleFullscreen
                clearBackground lightGray
                render allSystems w
                -- drawFPS 10 10
            }


driveFood :: IO ()
driveFood =
    initFoodWorld >>= gameLoop foodSysWrapped windowShouldClose