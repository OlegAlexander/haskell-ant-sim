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
import qualified Data.Vector                        as V
import           Debug.Trace                        (traceShow)
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           System.Random                      (newStdGen, randoms)


-- ----------------------------------- ECS ---------------------------------- --

type Position = (Float, Float)
type Angle = Float -- in radians
type Speed = Float

data EntityType = BlackAnt | RedAnt deriving (Eq, Show)

data World = World {
    wTypes     :: V.Vector EntityType,
    wPositions :: V.Vector (Maybe Position),
    wAngles    :: V.Vector (Maybe Angle),
    wSpeeds    :: V.Vector (Maybe Speed)
} deriving (Eq, Show)

initWorld :: World
initWorld = World {
    wTypes     = V.fromList [BlackAnt, RedAnt],
    wPositions = V.fromList [Just (0, 0), Just (10, 10)],
    wAngles    = V.fromList [Just 0, Just (pi/2)],
    wSpeeds    = V.fromList [Just 3, Just 2]
}

calcMovement :: Position -> Angle -> Speed -> Position
calcMovement (x, y) angle speed = (x + speed * cos angle, y + speed * sin angle)

updateMovement :: V.Vector (Maybe Position)
                  -> V.Vector (Maybe Angle)
                  -> V.Vector (Maybe Speed)
                  -> V.Vector (Maybe Position)
updateMovement positions angles speeds =
    V.zipWith3 (\pos angle speed -> case (pos, angle, speed) of
        (Just p, Just a, Just s) -> Just (calcMovement p a s)
        _                        -> pos) positions angles speeds

updateWorld :: World -> World
updateWorld world@(World types positions angles speeds) =
    world { wPositions = updateMovement positions angles speeds}

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
makePicture (World types positions _ _) =
    let antPics = V.zipWith antToPicture types positions
    in pictures (V.toList antPics)
    where
        antToPicture :: EntityType -> Maybe Position -> Picture
        antToPicture BlackAnt (Just (x, y)) =
            antShapes
            & translate x y
        antToPicture RedAnt (Just (x, y)) =
            antShapes
            & color red
            & translate x y
        antToPicture _ _ = Blank

        antShapes :: Picture
        antShapes = circleSolid 10



handleEvent :: Event -> World -> World
handleEvent e world = id world


stepWorld :: Float -> World -> World
stepWorld _ world = updateWorld world

