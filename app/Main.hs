{-# LANGUAGE PatternGuards #-}
-- | Simple picture drawing application.
--   Like MSPaint, but you can only draw lines.
--
module Main where

import qualified Data.Matrix                        as M
import           Debug.Trace                        (traceShow)
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Model


tileSize :: Int
tileSize = 20

tileSizeF :: Float
tileSizeF = fromIntegral tileSize

gridWidth :: Int
gridWidth = 40

gridWidthF :: Float
gridWidthF = fromIntegral gridWidth

gridHeight :: Int
gridHeight = 30

gridHeightF :: Float
gridHeightF = fromIntegral gridHeight

main :: IO ()
main
 = do   putStrLn "Starting Ant Sim..."
        let state = initGrid gridWidth gridHeight
        play    (InWindow "Haskell Ant Sim" (gridWidth * tileSize, gridHeight * tileSize) (0,0))
                (greyN 0.5) 100 state
                makeGridPicture handleEvent stepWorld

-- | The game state.
data State
        = State (Maybe Path)    -- The current line being drawn.
                [Picture]       -- All the lines drawn previously.


-- | A Line Segment
type Segment    = ((Float, Float), (Float, Float))


-- | Convert our state to a picture.
makePicture :: State -> Picture
makePicture (State m xs)
        = Pictures (maybe xs (\x -> Line x : xs) m)


makeGridPicture :: Grid -> Picture
makeGridPicture g =
    let (w, h) = (M.ncols g, M.nrows g)
        tx = gridWidthF / 2 * tileSizeF - tileSizeF / 2
        ty = gridHeightF / 2 * tileSizeF - tileSizeF / 2
    in translate (-tx) ty (scale 1.0 (-1.0) (Pictures [patchToPicture (x-1) (y-1) (getPatch x y g) | y <- [1..h], x <- [1..w]]))
    where
        patchToPicture :: Int -> Int -> Patch -> Picture
        patchToPicture x y p = Translate (fromIntegral x * tileSizeF) (fromIntegral y * tileSizeF) $ case p of
            Border     -> Color black $ rectangleSolid tileSizeF tileSizeF
            Wall       -> Color black $ rectangleSolid tileSizeF tileSizeF
            Nest       -> Color red   $ rectangleSolid tileSizeF tileSizeF
            Food _     -> Color green $ rectangleSolid tileSizeF tileSizeF
            Ground _ _ -> Color white $ rectangleSolid tileSizeF tileSizeF



-- | Handle mouse click and motion events.
-- handleEvent :: Event -> State -> State
-- handleEvent event state
--         -- If the mouse has moved, then extend the current line.
--         | EventMotion (x, y)    <- event
--         , State (Just ps) ss    <- state
--         = State (Just ((x, y):ps)) ss

--         -- Start drawing a new line.
--         | EventKey (MouseButton LeftButton) Down _ pt@(x,y) <- event
--         , State Nothing ss       <- state
--         = State (Just [pt])
--                 ((Translate x y $ Scale 0.1 0.1 $ Text "Down") : ss)

--         -- Finish drawing a line, and add it to the picture.
--         | EventKey (MouseButton LeftButton) Up _ pt@(x,y)      <- event
--         , State (Just ps) ss    <- state
--         = State Nothing
--                 ((Translate x y $ Scale 0.1 0.1 $ Text "up") : Line (pt:ps) : ss)

--         | otherwise
--         = state




mouseToScreen :: (Float, Float) -> (Float, Float)
mouseToScreen (x, y) = ((gridWidthF * tileSizeF) / 2 + x, (gridHeightF * tileSizeF) / 2 - y)

screenToGrid :: (Float, Float) -> (Int, Int)
screenToGrid (x, y) = let (x', y') = (floor (x / tileSizeF) + 1, floor (y / tileSizeF) + 1)
                            in (max 1 (min gridWidth x'), max 1 (min gridHeight y'))

mouseToGrid :: (Float, Float) -> (Int, Int)
mouseToGrid = screenToGrid . mouseToScreen

handleEvent :: Event -> Grid -> Grid
handleEvent e g =
    case e of
        EventKey (MouseButton LeftButton) Down _ (x, y) -> let (x', y') = mouseToGrid (x, y) in drawPatch x' y' (Food 10) g
        _                                               -> g



-- stepWorld :: Float -> State -> State
-- stepWorld _ = id

stepWorld :: Float -> Grid -> Grid
stepWorld _ = id
