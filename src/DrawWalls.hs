module DrawWalls where

import Constants (antPng, minWallSize, wallColor)
import Control.Monad (forM_, when)
import Data.Maybe (fromJust, isJust, isNothing)
import GHC.Float (int2Float)
import Raylib.Core (
    clearBackground,
    getMousePosition,
    initWindow,
    isKeyPressed,
    setMouseCursor,
    setTargetFPS,
    windowShouldClose,
 )
import Raylib.Core.Shapes (drawRectangleLinesEx, drawRectangleRec)
import Raylib.Core.Text (drawText)
import Raylib.Core.Textures (loadTexture)
import Raylib.Types (KeyboardKey (KeyW), Rectangle (Rectangle), Vector2 (Vector2))
import Raylib.Types.Core (MouseCursor (MouseCursorCrosshair))
import Raylib.Util (drawing)
import Raylib.Util.Colors (blue, lightGray)
import Shared (gameLoop)
import System.Random (randomIO)
import Types (WallDrawingState (..), Walls (..), World (..))


getWallDrawingState :: Bool -> Maybe (Vector2, Vector2) -> WallDrawingState
getWallDrawingState wPressed wbd
    | wPressed && isNothing wbd = Started
    | not wPressed && isJust wbd = InProgress
    | wPressed && isJust wbd = Finished
    | otherwise = Idle


calcBoundingBox :: Vector2 -> Vector2 -> Rectangle
calcBoundingBox (Vector2 x1 y1) (Vector2 x2 y2) =
    Rectangle (min x1 x2) (min y1 y2) (abs (x1 - x2)) (abs (y1 - y2))


bigEnough :: Rectangle -> Bool
bigEnough (Rectangle _ _ w h) = w > minWallSize && h > minWallSize


initWallsWorld :: IO World
initWallsWorld = do
    window <- initWindow 1000 800 "Draw Walls"
    setTargetFPS 60
    setMouseCursor MouseCursorCrosshair
    antTexture <- loadTexture antPng window
    let walls = Walls [] Nothing
    return $ World window antTexture [] True walls


handleWallInput :: World -> IO World
handleWallInput w = do
    wPressed <- isKeyPressed KeyW
    let walls = wWalls w
        wbd = wallBeingDrawn walls
        status = getWallDrawingState wPressed wbd
    case status of
        Idle -> return w
        Started -> do
            mousePos <- getMousePosition
            return w{wWalls = walls{wallBeingDrawn = Just (mousePos, mousePos)}}
        InProgress -> do
            mousePos <- getMousePosition
            return w{wWalls = walls{wallBeingDrawn = Just (fst $ fromJust wbd, mousePos)}}
        Finished -> do
            let (start, end) = fromJust wbd
                newWall = calcBoundingBox start end
            if bigEnough newWall
                then
                    return w{wWalls = walls{unWalls = newWall : unWalls walls, wallBeingDrawn = Nothing}}
                else
                    return w{wWalls = walls{unWalls = unWalls walls, wallBeingDrawn = Nothing}}


updateWallsWorld :: World -> World
updateWallsWorld = id


renderWallsWorld :: World -> IO ()
renderWallsWorld w = do
    let walls = wWalls w
    drawing $ do
        clearBackground lightGray
        drawText "Press 'w' to draw walls" 10 10 30 blue
        forM_ (unWalls walls) $ \wall -> drawRectangleRec wall wallColor
        when (isJust $ wallBeingDrawn walls) $ do
            let (start, end) = fromJust $ wallBeingDrawn walls
                wall = calcBoundingBox start end
            drawRectangleRec wall wallColor
            drawRectangleLinesEx wall 2 blue


driveDrawWalls :: IO ()
driveDrawWalls =
    initWallsWorld >>= gameLoop handleWallInput updateWallsWorld renderWallsWorld windowShouldClose
