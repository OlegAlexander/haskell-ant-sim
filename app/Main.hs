-- Short stack trace: cabal run --enable-profiling --ghc-options="-prof -fprof-auto"
--  Long stack trace: cabal run --enable-profiling --ghc-options="-prof -fprof-auto" haskell-ant-sim -- +RTS -xc
-- Disable profiling: cabal run --disable-profiling

{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
-- | Simple picture drawing application.
--   Like MSPaint, but you can only draw lines.
--
module Main where

import qualified Data.Matrix                        as M
import           Debug.Trace                        (traceShow)
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Model
import           System.Random                      (newStdGen)


tileSize :: Int
tileSize = 10

tileSizeF :: Float
tileSizeF = fromIntegral tileSize

gridWidth :: Int
gridWidth = 80

gridWidthF :: Float
gridWidthF = fromIntegral gridWidth

gridHeight :: Int
gridHeight = 60

gridHeightF :: Float
gridHeightF = fromIntegral gridHeight

numAnts :: Int
numAnts = 25

fps :: Int
fps = 15

main :: IO ()
main = do
    gen <- newStdGen
    let state = initState gridWidth gridHeight numAnts gen
        (w, h) = (gridWidth * tileSize, gridHeight * tileSize)
    play (InWindow "Haskell Ant Sim" (w, h) (0,0))
         (greyN 0.1) fps state makePicture handleEvent stepWorld



patchColor :: Patch -> Color
patchColor p = case p of
    Border     -> white
    Wall       -> white
    Nest       -> red
    Food u     -> makeColor 0 (u / 100) 0 1
    Ground f n -> mixColors 0.5 0.5
                  (makeColor 0 (min 0.5 (f / 1000)) 0 1)
                  (makeColor (min 0.5 (n / 1000)) 0 0 1)



makePicture :: State -> Picture
makePicture (g, ants, gen) =
    let (w, h) = (M.ncols g, M.nrows g)
        tx = gridWidthF / 2 * tileSizeF - tileSizeF / 2
        ty = gridHeightF / 2 * tileSizeF - tileSizeF / 2
        patchPics = [patchToPicture (x-1) (y-1) (getPatch x y g) | y <- [1..h], x <- [1..w]]
        antsPics = [antToPicture a | a <- ants]
    in translate (-tx) ty (scale 1.0 (-1.0) (pictures (patchPics ++ antsPics)))
    where
        patchToPicture :: Int -> Int -> Patch -> Picture
        patchToPicture x y p = translate (fromIntegral x * tileSizeF) (fromIntegral y * tileSizeF) $ color (patchColor p) $ rectangleSolid tileSizeF tileSizeF

        antToPicture :: Ant -> Picture
        antToPicture (Ant id' x y dir mode f n) = translate (fromIntegral (x-1) * tileSizeF) (fromIntegral (y-1) * tileSizeF) $ color (dim orange) $ circleSolid (tileSizeF / 2)



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


handleEvent :: Event -> State -> State
handleEvent e (g, ants, gen) = case e of
    EventKey (MouseButton LeftButton) Down (Modifiers Down _ _) (x, y) -> let (x', y') = mouseToGrid (x, y) in (drawPatch x' y' (Ground 0 0) g, ants, gen)
    EventKey (MouseButton LeftButton)  Down _ (x, y) -> let (x', y') = mouseToGrid (x, y) in (drawPatch x' y' Wall g, ants, gen)
    EventKey (MouseButton RightButton) Down _ (x, y) -> let (x', y') = mouseToGrid (x, y) in (drawPatch x' y' (Food 100) g, ants, gen)
    _    -> (g, ants, gen)


stepWorld :: Float -> State -> State
stepWorld _ = updateState
