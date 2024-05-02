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
import           AntSim                             (pheromoneAmount)
import           Data.Function                      ((&))
import           Data.List                          (foldl', transpose, unzip5,
                                                     zip5)
import qualified Data.Vector                        as V
import           Data.Vector.Generic                (foldl', new)
import           Debug.Trace                        (traceShow)
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           System.Random                      (newStdGen, randoms)


-- ----------------------------------- ECS ---------------------------------- --

type Position = (Float, Float)
type Angle = Float -- in radians
type Speed = Float
type PheremoneAmount = Float

data Archetype = BlackAnt | PheremoneDrop deriving (Eq, Show)

data World = World {
    wTypes           :: [Archetype],
    wPositions       :: [Maybe Position],
    wAngles          :: [Maybe Angle],
    wSpeeds          :: [Maybe Speed],
    wPheremoneAmount :: [Maybe PheremoneAmount]
} deriving (Eq, Show)


initWorld :: World
initWorld =
    let world = World [] [] [] [] []
        entities = [newBlackAnt (0, 0) (pi/4) 3, newPheremoneDrop (10, 10) 5]
    in Data.List.foldl' concatWorlds world entities

concatWorlds :: World -> World -> World
concatWorlds targetWorld sourceWorld = World {
    wTypes           = wTypes targetWorld ++ wTypes sourceWorld,
    wPositions       = wPositions targetWorld ++ wPositions sourceWorld,
    wAngles          = wAngles targetWorld ++ wAngles sourceWorld,
    wSpeeds          = wSpeeds targetWorld ++ wSpeeds sourceWorld,
    wPheremoneAmount = wPheremoneAmount targetWorld ++ wPheremoneAmount sourceWorld

}

newBlackAnt :: Position -> Angle -> Speed -> World
newBlackAnt pos angle speed = World {
    wTypes     = [BlackAnt],
    wPositions = [Just pos],
    wAngles    = [Just angle],
    wSpeeds    = [Just speed],
    wPheremoneAmount = [Nothing]
}

newPheremoneDrop :: Position -> PheremoneAmount -> World
newPheremoneDrop pos amount = World {
    wTypes     = [PheremoneDrop],
    wPositions = [Just pos],
    wAngles    = [Nothing],
    wSpeeds    = [Nothing],
    wPheremoneAmount = [Just amount]
}

calcMovement :: Position -> Angle -> Speed -> Position
calcMovement (x, y) angle speed = (x + speed * cos angle, y + speed * sin angle)

movementSystem :: World -> World
movementSystem world =
    -- Notice how we pull out the fields we need
    let positions = wPositions world
        angles = wAngles world
        speeds = wSpeeds world
        positions' = zipWith3 (\pos angle speed -> case (pos, angle, speed) of
            (Just p, Just a, Just s) -> Just $ calcMovement p a s
            _                        -> pos) positions angles speeds
    in world { wPositions = positions' }

updateWorld :: World -> World
updateWorld world = movementSystem world

-- -------------------------------------------------------------------------- --


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
    play (InWindow "ECS FTW!" (screenWidth, screenHeight) (20,20))
         (greyN 0.8) fps initWorld makePicture handleEvent stepWorld


makePicture :: World -> Picture
makePicture (World types positions _ _ amounts) =
    let antPics = zipWith antToPicture types positions
    in pictures antPics
    where
        antToPicture :: Archetype -> Maybe Position -> Picture
        antToPicture BlackAnt (Just (x, y)) =
            antShapes
            & translate x y
        antToPicture PheremoneDrop (Just (x, y)) =
            rectangleSolid 10 10
            & color green
            & translate x y
        antToPicture _ _ = Blank

        antShapes :: Picture
        antShapes = circleSolid 5



handleEvent :: Event -> World -> World
handleEvent e world = id world


stepWorld :: Float -> World -> World
stepWorld _ world = updateWorld world

