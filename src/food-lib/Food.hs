{-# HLINT ignore "Eta reduce" #-}

module Food where

import AntMovement (antMovementSys)
import Constants (
    antAcceleration,
    antMaxSpeed,
    antPng,
    antTurnAngle,
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
    initWindow,
    isKeyDown,
    isKeyPressed,
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
import Raylib.Types (
    KeyboardKey (..),
    MouseCursor (MouseCursorCrosshair),
    Rectangle (Rectangle),
    TraceLogLevel (LogWarning),
 )
import Raylib.Types.Core (Vector2 (..))
import Raylib.Util (drawing)
import Raylib.Util.Colors (black, blue, brown, green, lightGray, red)
import Shared (System (..), gameLoop, getNextPos)
import System.Random (mkStdGen)
import Types (Ant (..), EntityType (..), GoDir (..), Mode (..), Sprite (..), WheelPos (..), World (..))


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
        playerAnt = Ant antPos 0 0 SeekFood rng Stop Center LeftSprite [] 0 0
    return $ World window antTexture playerAnt nestPos True True False True walls Nothing


handleFoodInput :: World -> IO World
handleFoodInput w = return w


updateFoodWorld :: World -> World
updateFoodWorld w = w


renderFoodWorld :: World -> IO ()
renderFoodWorld w = return ()


foodSys :: System World
foodSys = System handleFoodInput updateFoodWorld renderFoodWorld


foodSysWrapped :: System World
foodSysWrapped =
    let allSystems = antMovementSys <> foodSys
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