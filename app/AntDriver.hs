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


screenWidth :: Int
screenWidth = 1200

screenHeight :: Int
screenHeight = 800

screenWidthF :: Float
screenWidthF = fromIntegral screenWidth

screenHeightF :: Float
screenHeightF = fromIntegral screenHeight

numAnts :: Int
numAnts = 10

fps :: Int
fps = 30

antScale :: Float
antScale = 1

antStepSize :: Float
antStepSize = 3

antAngleRange :: Float
antAngleRange = pi / 15 -- 15: 12, 20:9, 30:6 degrees

antAccelerationRange :: Float
antAccelerationRange = 0.1

main :: IO ()
main = do
    gen <- newStdGen
    let seeds = (randoms gen :: [Int]) & take numAnts
        ants = mkAnts 0 0 seeds
    play (InWindow "Ant Driver" (screenWidth, screenHeight) (20,20))
         (greyN 0.9) fps ants makePicture handleEvent stepWorld


makePicture :: [Ant] -> Picture
makePicture ants =
    let antPics = map antToPicture ants
        in pictures antPics -- (antPics ++ nestPicture)
    where
        antToPicture :: Ant -> Picture
        antToPicture (Ant x y theta speed mode gen) =
            antShapes
            & scale antScale antScale
            & rotate (theta * (180 / pi) * (-1))
            & translate x y

        antShapes :: Picture
        antShapes =
            let head'   = circleSolid 2.5 & scale 1 0.8 & translate 5.6 0
                thorax  = circleSolid 2 & scale 1.6 0.6 & translate 0 0
                abdomen = circleSolid 2 & scale 1.8 1.3 & translate (-6.6) 0
            in pictures [head', thorax, abdomen]

        nestPicture :: [Picture]
        nestPicture = [circleSolid 2 & color red]



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
stepWorld _ ants =
    map (wrapAroundAnt screenWidthF screenHeightF
        . moveAntRandomly antStepSize antAngleRange antAccelerationRange) ants
