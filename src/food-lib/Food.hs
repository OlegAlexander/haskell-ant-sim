{-# HLINT ignore "Eta reduce" #-}

module Food where

import AntMovement (antMovementSys)
import Constants (
    antAcceleration,
    antMaxSpeed,
    antPng,
    antTurnAngle,
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
    drawRectangleRec,
 )
import Raylib.Core.Text (drawFPS)
import Raylib.Core.Textures (loadTexture)
import Raylib.Types (KeyboardKey (..), MouseButton (MouseButtonLeft), MouseCursor (MouseCursorCrosshair), Rectangle (Rectangle), TraceLogLevel (LogWarning))
import Raylib.Types.Core (Vector2 (..))
import Raylib.Util (drawing)
import Raylib.Util.Colors (black, blue, brown, green, lightGray, red)
import Shared (System (..), gameLoop, getNextPos)
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


handleFoodInput :: World -> IO World
handleFoodInput w = do
    -- When the mouse is clicked, add a Food object at that position to foodBeingDrawn.
    -- Continue incrementing foodAmount while the mouse is held down.
    -- When the mouse is released, stop incrementing foodAmount and add the Food object to the food list.
    mousePos <- getMousePosition
    isMouseLeftPressed <- isMouseButtonDown MouseButtonLeft
    let fbd = wFoodBeingDrawn w
    case (isMouseLeftPressed, fbd) of
        (True, Nothing) -> return $ w{wFoodBeingDrawn = Just (Food mousePos foodGrowthAmount)}
        (True, Just (Food pos amount)) -> return $ w{wFoodBeingDrawn = Just (Food pos (amount + foodGrowthAmount))}
        (False, Just food) -> return $ w{wFoodBeingDrawn = Nothing, wFood = food : wFood w}
        (False, Nothing) -> return w


updateFoodWorld :: World -> World
updateFoodWorld w = w


renderFoodWorld :: World -> IO ()
renderFoodWorld w = do
    forM_ (wFood w) $ \(Food pos amount) -> do
        -- Draw food as a circle at foodPos with a radius of foodAmount * foodScale
        let radius = int2Float amount * foodScale
        drawCircleV pos radius foodColor

        -- Draw a rectangle inscribed inside the circle
        -- TODO Do this the other way around:
        -- the rectangle should be the true radius of the food, and the circle should be circumscribed around it
        let (Vector2 x y) = pos
            side = (radius * 2) / sqrt 2
            box = Rectangle (x - side / 2) (y - side / 2) side side
        drawRectangleRec box black

    -- Also draw foodBeingDrawn the same way
    case wFoodBeingDrawn w of
        Just (Food pos amount) -> drawCircleV pos (int2Float amount * foodScale) foodColor
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