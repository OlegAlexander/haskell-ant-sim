module DrawWalls where

import Control.Monad (forM_, when)
import Data.Maybe (fromJust, isJust, isNothing)
import Raylib.Core (clearBackground, getMousePosition, initWindow, isKeyPressed, setMouseCursor, setTargetFPS, windowShouldClose)
import Raylib.Core.Shapes (drawRectangleLinesEx, drawRectangleRec)
import Raylib.Types (Color, KeyboardKey (KeyW), Rectangle (Rectangle), Vector2 (Vector2))
import Raylib.Types.Core (MouseCursor (MouseCursorCrosshair))
import Raylib.Util (drawing)
import Raylib.Util.Colors (blue, lightGray, white)
import Shared (gameLoop)


wallColor :: Color
wallColor = white


data Walls = Walls
    { walls :: [Rectangle],
      wallBeingDrawn :: Maybe (Vector2, Vector2)
    }
    deriving (Eq, Show)


data WallDrawingStatus = Idle | Started | InProgress | Finished
    deriving (Eq, Show)


getWallDrawingStatus :: Bool -> Maybe (Vector2, Vector2) -> WallDrawingStatus
getWallDrawingStatus wPressed wbd
    | wPressed && isNothing wbd = Started
    | not wPressed && isJust wbd = InProgress
    | wPressed && isJust wbd = Finished
    | otherwise = Idle


calcBoundingBox :: Vector2 -> Vector2 -> Rectangle
calcBoundingBox (Vector2 x1 y1) (Vector2 x2 y2) =
    Rectangle (min x1 x2) (min y1 y2) (abs (x1 - x2)) (abs (y1 - y2))


initWallsWorld :: IO Walls
initWallsWorld = do
    _ <- initWindow 800 600 "Draw Walls Driver"
    setTargetFPS 60
    setMouseCursor MouseCursorCrosshair
    return (Walls [] Nothing)


handleWallInput :: Walls -> IO Walls
handleWallInput w = do
    wPressed <- isKeyPressed KeyW
    let wbd = wallBeingDrawn w
        status = getWallDrawingStatus wPressed wbd
    case status of
        Idle -> return w
        Started -> do
            mousePos <- getMousePosition
            return w{wallBeingDrawn = Just (mousePos, mousePos)}
        InProgress -> do
            mousePos <- getMousePosition
            return w{wallBeingDrawn = Just (fst $ fromJust wbd, mousePos)}
        Finished -> do
            let (start, end) = fromJust wbd
                newWall = calcBoundingBox start end
            return w{walls = newWall : walls w, wallBeingDrawn = Nothing}


updateWallsWorld :: Walls -> Walls
updateWallsWorld = id


renderWallsWorld :: Walls -> IO ()
renderWallsWorld w = do
    drawing $ do
        clearBackground lightGray
        forM_ (walls w) $ \wall -> drawRectangleRec wall wallColor
        when (isJust $ wallBeingDrawn w) $ do
            let (start, end) = fromJust $ wallBeingDrawn w
                wall = calcBoundingBox start end
            drawRectangleRec wall wallColor
            drawRectangleLinesEx wall 3 blue


driveDrawWalls :: IO ()
driveDrawWalls = initWallsWorld >>= gameLoop handleWallInput updateWallsWorld renderWallsWorld windowShouldClose
