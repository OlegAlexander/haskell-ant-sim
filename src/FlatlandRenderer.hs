{-# HLINT ignore "Eta reduce" #-}

module FlatlandRenderer where

import Control.Monad (forM_, when)
import Data.Maybe (fromJust, isJust, isNothing)
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
import Raylib.Types (Color, KeyboardKey (KeyW), Rectangle (Rectangle), Vector2 (Vector2))
import Raylib.Types.Core (MouseCursor (MouseCursorCrosshair))
import Raylib.Util (drawing)
import Raylib.Util.Colors (blue, lightGray, white)
import Shared (gameLoop)


data Walls = Walls
    { walls :: [Rectangle],
      wallBeingDrawn :: Maybe (Vector2, Vector2)
    }
    deriving (Eq, Show)


data WallDrawingState = Idle | Started | InProgress | Finished
    deriving (Eq, Show)


initFlatlandRendererWorld :: IO Walls
initFlatlandRendererWorld = do
    _ <- initWindow 1000 800 "Flatland Renderer Driver"
    setTargetFPS 60
    setMouseCursor MouseCursorCrosshair
    return (Walls [] Nothing)


handleFlatlandRendererInput :: Walls -> IO Walls
handleFlatlandRendererInput w = return w


updateFlatlandRendererWorld :: Walls -> Walls
updateFlatlandRendererWorld = id


renderFlatlandRendererWorld :: Walls -> IO ()
renderFlatlandRendererWorld w = do
    drawing $ do
        clearBackground lightGray


driveFlatlandRenderer :: IO ()
driveFlatlandRenderer =
    initFlatlandRendererWorld >>= gameLoop handleFlatlandRendererInput updateFlatlandRendererWorld renderFlatlandRendererWorld windowShouldClose
