{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use forM_" #-}

module Food where

import AntMovement (antMovementSys)
import Constants (antAcceleration, antMaxSpeed, antPng, antTurnAngle, collisionRectSize, foodColor, foodGrowthAmount, foodScale, fps, nestSize, screenHeight, screenWidth)
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
import Types (Ant (..), EntityType (..), Food (..), GoDir (..), Mode (..), Nest (..), Sprite (..), WheelPos (..), World (..))


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
        nest = Nest antPos 0 (calcCenteredRect antPos collisionRectSize)
        playerAnt = Ant antPos 0 0 SeekFood rng Stop Center LeftSprite [] 0 0 False
    return $ World window antTexture playerAnt nest True True False True walls Nothing [] Nothing


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
updateFoodWorld w =
    -- If the ant enters a food rectangle and antHasFood is False, set antHasFood to True
    let ant = wPlayerAnt w
        foods = wFood w
        antInFoods = any (isPointInRect (antPos ant) . foodCollisionRect) foods

        -- If an ant has food and enters the nest rectangle, set antHasFood to False
        nestRect = nestCollisionRect (wNest w)
        antInNest = isPointInRect (antPos ant) nestRect

        antHasFood' = case (antHasFood ant, antInNest, antInFoods) of
            (False, _, True) -> True
            (True, True, _) -> False
            _ -> antHasFood ant

        -- TODO decrement food when ant touches it
        -- Increment nest food amount when ant touches it with food

        _ = traceShowId ("antHasFood ant", antHasFood ant, "antInNest", antInNest, "antInFoods", antInFoods)
    in  w{wPlayerAnt = ant{antHasFood = antHasFood'}}


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
    let nest = wNest w

    -- draw the nest
    drawCircleV (nestPos nest) nestSize brown

    -- draw nest rect
    drawRectangleLinesEx (nestCollisionRect nest) 2 black

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