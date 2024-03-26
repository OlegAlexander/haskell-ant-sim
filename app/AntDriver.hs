-- Short stack trace: cabal run --enable-profiling --ghc-options="-prof -fprof-auto"
--  Long stack trace: cabal run --enable-profiling --ghc-options="-prof -fprof-auto" haskell-ant-sim -- +RTS -xc
-- Disable profiling: cabal run --disable-profiling

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
-- | Simple picture drawing application.
--   Like MSPaint, but you can only draw lines.
--
module Main where

import           Ant
import           Data.Function                      ((&))
import           Debug.Trace                        (traceShow)
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           System.Random                      (newStdGen, randoms)


numAnts :: Int
numAnts = 500

fps :: Int
fps = 30

main :: IO ()
main = do
    gen <- newStdGen
    let seeds = take numAnts $ randoms gen :: [Int]
        ants = mkAnts 0 0 seeds
    -- printAnts ants
    play (InWindow "Ant Driver" (1200, 800) (20,20))
         (greyN 0.8) fps ants makePicture handleEvent stepWorld


makePicture :: [Ant] -> Picture
makePicture ants =
    let antPics = map antToPicture ants
        nestPic = [nestToPicture]
        in pictures (antPics ++ nestPic)
    where
        antToPicture :: Ant -> Picture
        antToPicture (Ant x y theta mode gen) =
            let antPolygon = [(100, -50/4), (-100, -50), (-100, 50), (100, 50/4)]
            in
                polygon antPolygon
                & scale 0.2 0.2
                & rotate (theta * (180 / pi) * (-1))
                & translate x y

        nestToPicture :: Picture
        nestToPicture = circleSolid 3 & color red



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



handleEvent :: Event -> [Ant] -> [Ant]
handleEvent _ ants = id ants -- case e of
    -- EventKey (MouseButton LeftButton) Down (Modifiers Down _ _) (x, y) -> let (x', y') = mouseToGrid (x, y) in (drawPatch x' y' (Ground 0 0) g, ants, gen)
    -- EventKey (MouseButton LeftButton)  Down _ (x, y) -> let (x', y') = mouseToGrid (x, y) in (drawPatch x' y' Wall g, ants, gen)
    -- EventKey (MouseButton RightButton) Down _ (x, y) -> let (x', y') = mouseToGrid (x, y) in (drawPatch x' y' (Food 100) g, ants, gen)
    -- _    -> (g, ants, gen)


stepWorld :: Float -> [Ant] -> [Ant]
stepWorld _ ants = map (stepAnt 2) ants
