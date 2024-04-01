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
antScale = 2

antMaxSpeed :: Float
antMaxSpeed = 4

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
        antToPicture (Ant x y theta speed mode gen stopGo wheelPos sprite) =
            let sprite' = case sprite of
                    LeftSprite  -> antShapes
                    RightSprite -> antShapes & scale 1 (-1)
            in  sprite'
                & scale antScale antScale
                & rotate (theta * (180 / pi) * (-1))
                & translate x y

        antShapes :: Picture
        antShapes =
            let head'   = circleSolid 1 & scale 1.5 1.35 & translate 3.6 0
                thorax  = circleSolid 1 & scale 2.5 0.7 & translate 0 0
                abdomen = circleSolid 1 & scale 2.5 1.5 & translate (-4.8) 0
                antenna1L = circleSolid 1 & scale 1.47 0.17 & translate 4.2 (-3.45) & rotate (-58)
                antenna2L = circleSolid 1 & scale 2.0 0.17 & translate 8.4 (0.1) & rotate (-25)
                antenna1R = circleSolid 1 & scale 1.47 0.17 & translate 4.2 (3.45) & rotate (58)
                antenna2R = circleSolid 1 & scale 2.0 0.17 & translate 8.4 (-0.1) & rotate (25)
                frontLeg1L = circleSolid 1 & scale 1.47 0.25 & translate 0.99 (0.16) & rotate (-58)
                frontLeg2L = circleSolid 1 & scale 1.47 0.25 & translate 2.78 (1.39) & rotate (-23)
                frontLeg3L = circleSolid 1 & scale 1.2 0.15 & translate 3.86 (2.3) & rotate (-8.5)
                midLeg1L = circleSolid 1 & scale 2.0 0.25 & translate 2.0 (0.75) & rotate (-85)
                midLeg2L = circleSolid 1 & scale 1.47 0.25 & translate 5.0 (-0.45) & rotate (-103)
                midLeg3L = circleSolid 1 & scale 1.2 0.15 & translate 7.23 (-1.57) & rotate (-113)
                backLeg1L = circleSolid 1 & scale 1.8 0.25 & translate 1.59 (1.25) & rotate (-96)
                backLeg2L = circleSolid 1 & scale 1.47 0.25 & translate 4.33 (-1.05) & rotate (-138)
                backLeg3L = circleSolid 1 & scale 1.8 0.15 & translate 4.88 (-4.56) & rotate (179)
                frontLeg1R = circleSolid 1 & scale 1.47 0.25 & translate 1.85 (-0.53) & rotate (38.5)
                frontLeg2R = circleSolid 1 & scale 1.47 0.25 & translate 4.44 (-0.89) & rotate (33)
                frontLeg3R = circleSolid 1 & scale 1.2 0.15 & translate 6.46 (-1.85) & rotate (22)
                midLeg1R = circleSolid 1 & scale 2.0 0.25 & translate 0.96 (-0.91) & rotate (56)
                midLeg2R = circleSolid 1 & scale 1.47 0.25 & translate 3.9 (0.76) & rotate (91.2)
                midLeg3R = circleSolid 1 & scale 1.2 0.15 & translate 5.27 (2.58) & rotate (113)
                backLeg1R = circleSolid 1 & scale 1.8 0.25 & translate 3.06 (-0.2) & rotate (146)
                backLeg2R = circleSolid 1 & scale 1.47 0.25 & translate 5.8 (0.12) & rotate (150)
                backLeg3R = circleSolid 1 & scale 1.8 0.15 & translate 8.15 (2.05) & rotate (166)
            in pictures [head', thorax, abdomen, antenna1L, antenna2L, antenna1R, antenna2R,
                        frontLeg1L, frontLeg2L, frontLeg3L, midLeg1L, midLeg2L, midLeg3L, backLeg1L, backLeg2L, backLeg3L,
                        frontLeg1R, frontLeg2R, frontLeg3R, midLeg1R, midLeg2R, midLeg3R, backLeg1R, backLeg2R, backLeg3R]

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
                   & driveAnt antStepSize 0.25 0.25 antMaxSpeed (pi/15) (pi/60)
                   & cycleAntSprite antMaxSpeed
                   & wrapAroundAnt screenWidthF screenHeightF
        movedOtherAnts = map (wrapAroundAnt screenWidthF screenHeightF
            . moveAntRandomly antStepSize antAngleRange antAccelerationRange) (tail ants)
    in movedAnt : movedOtherAnts
