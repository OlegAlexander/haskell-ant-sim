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
numAnts = 1

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
         (greyN 0.8) fps ants makePicture handleEvent stepWorld


makePicture :: [Ant] -> Picture
makePicture ants =
    let antPics = map antToPicture ants
        in pictures antPics -- (antPics ++ nestPicture)
    where
        antToPicture :: Ant -> Picture
        antToPicture (Ant x y theta speed mode gen stopGo wheelPos) =
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


handleEvent :: Event -> [Ant] -> [Ant]
handleEvent e ants = case e of
    EventKey (SpecialKey KeyUp)    Down _ _ -> let ant = head ants in ant {antStopGo = Go } : tail ants
    EventKey (SpecialKey KeyUp)    Up _ _   -> let ant = head ants in ant {antStopGo = Neutral } : tail ants
    EventKey (SpecialKey KeyDown)  Down _ _ -> let ant = head ants in ant {antStopGo = Stop } : tail ants
    EventKey (SpecialKey KeyDown)  Up _ _   -> let ant = head ants in ant {antStopGo = Neutral } : tail ants
    EventKey (SpecialKey KeyLeft)  Down _ _ -> let ant = head ants in ant {antWheelPos = Ant.Left } : tail ants
    EventKey (SpecialKey KeyLeft)  Up _ _   -> let ant = head ants in ant {antWheelPos = Center } : tail ants
    EventKey (SpecialKey KeyRight) Down _ _ -> let ant = head ants in ant {antWheelPos = Ant.Right } : tail ants
    EventKey (SpecialKey KeyRight) Up _ _   -> let ant = head ants in ant {antWheelPos = Center } : tail ants
    _                                       -> ants


stepWorld :: Float -> [Ant] -> [Ant]
stepWorld _ ants =
    let ant = head ants
        movedAnt = ant
                   & driveAnt antStepSize 0.1 0.2 3 (pi/15) (pi/30)
                   & wrapAroundAnt screenWidthF screenHeightF
        movedOtherAnts = map (wrapAroundAnt screenWidthF screenHeightF
            . moveAntRandomly antStepSize antAngleRange antAccelerationRange) (tail ants)
    in movedAnt : movedOtherAnts
