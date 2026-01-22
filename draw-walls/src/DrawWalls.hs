{-# LANGUAGE OverloadedRecordDot #-}

{-# HLINT ignore "Use <$>" #-}

module DrawWalls where

import Constants (
    minWallSize,
    wallColor,
    screenWidth,
    screenHeight
 )
import Control.Monad (forM_, when)
import Data.Function ((&))
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Sequence ((<|))
import Data.Sequence qualified as Seq
import Raylib.Core (
    clearBackground,
    getMousePosition,
    initWindow,
    isKeyPressed,
    isMouseButtonDown,
    setMouseCursor,
    setTargetFPS,
    windowShouldClose,
 )
import Raylib.Core.Shapes (drawRectangleLinesEx, drawRectangleRec)
import Raylib.Core.Text (drawText)
import Raylib.Types (
    KeyboardKey (KeyW),
    Rectangle (Rectangle),
    Vector2 (Vector2),
 )
import Raylib.Types.Core (MouseButton (MouseButtonRight), MouseCursor (MouseCursorCrosshair))
import Raylib.Util (drawing)
import Raylib.Util.Colors (blue, lightGray)
import Shared (System (..), defaultWorld, gameLoop, isPointInRect)
import System.Random (newStdGen, randomIO)
import Types (
    WallDrawingState (..),
    World (..),
 )


getWallDrawingState :: Bool -> Bool -> Maybe (Vector2, Vector2) -> WallDrawingState
getWallDrawingState wPressed isMouseRightPressed wbd
    | isMouseRightPressed = Deleted
    | wPressed && isNothing wbd = Started
    | not wPressed && isJust wbd = InProgress
    | wPressed && isJust wbd = Finished
    | otherwise = Idle


calcBoundingBox :: Vector2 -> Vector2 -> Rectangle
calcBoundingBox (Vector2 !x1 !y1) (Vector2 !x2 !y2) =
    Rectangle (min x1 x2) (min y1 y2) (abs (x1 - x2)) (abs (y1 - y2))


bigEnough :: Rectangle -> Bool
bigEnough (Rectangle _ _ !w !h) = w > minWallSize && h > minWallSize


initWallsWorld :: IO World
initWallsWorld = do
    _ <- initWindow screenWidth screenHeight "Draw Walls"
    setTargetFPS 60
    setMouseCursor MouseCursorCrosshair
    rng <- newStdGen
    return (defaultWorld rng)


handleWallInput :: World -> IO World
handleWallInput w = do
    wPressed <- isKeyPressed KeyW
    isMouseRightPressed <- isMouseButtonDown MouseButtonRight
    mousePos <- getMousePosition
    let walls = w.wWalls
        wbd = w.wWallBeingDrawn
        status = getWallDrawingState wPressed isMouseRightPressed wbd
    case status of
        Idle -> return w
        Started ->
            return
                w
                    { wWalls = walls,
                      wWallBeingDrawn =
                        Just (mousePos, mousePos)
                    }
        InProgress ->
            return
                w
                    { wWalls = walls,
                      wWallBeingDrawn =
                        Just (fst $ fromJust wbd, mousePos)
                    }
        Finished -> do
            let (!start, !end) = fromJust wbd
                newWall = calcBoundingBox start end
            if bigEnough newWall
                then
                    return w{wWalls = newWall <| walls, wWallBeingDrawn = Nothing}
                else
                    return w{wWalls = walls, wWallBeingDrawn = Nothing}
        Deleted -> do
            let wallsToKeep = walls & Seq.filter (not . isPointInRect mousePos)
            return w{wWalls = wallsToKeep}


renderWallsWorld :: World -> IO ()
renderWallsWorld w = do
    let walls = w.wWalls
        wbd = w.wWallBeingDrawn
    forM_ walls $ \wall -> drawRectangleRec wall wallColor
    when (isJust wbd) $ do
        let (!start, !end) = fromJust wbd
            wall = calcBoundingBox start end
        drawRectangleLinesEx wall 3 blue


drawWallsSys :: System World
drawWallsSys =
    mempty
        { handleInput = handleWallInput,
          render = renderWallsWorld
        }


drawWallsSysWrapped :: System World
drawWallsSysWrapped =
    drawWallsSys
        { render = \w -> drawing $ do
            clearBackground lightGray
            drawText "Press w to draw walls" 25 25 30 blue
            drawWallsSys.render w
        }


driveDrawWalls :: IO ()
driveDrawWalls =
    initWallsWorld >>= gameLoop drawWallsSysWrapped windowShouldClose
