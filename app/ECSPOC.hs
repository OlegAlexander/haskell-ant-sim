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
import           Data.List                          (transpose, unzip5, zip5)
import qualified Data.Vector                        as V
import           Data.Vector.Generic                (new)
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


-- To maintain type safety, you must add new components to World, NOT Entity!
-- Need TemplateHaskell to generate World? Hopefully there's an easier way...
-- Maybe use Tuples instead of Records? Tuples are easier to unzip/zip.
data World = World {
    wTypes           :: [Archetype],
    wPositions       :: [Maybe Position],
    wAngles          :: [Maybe Angle],
    wSpeeds          :: [Maybe Speed],
    wPheremoneAmount :: [Maybe PheremoneAmount]
} deriving (Eq, Show)

data Entity = Entity {
    eType            :: Archetype,
    ePosition        :: Maybe Position,
    eAngle           :: Maybe Angle,
    eSpeed           :: Maybe Speed,
    ePheremoneAmount :: Maybe PheremoneAmount
} deriving (Eq, Show)

type WorldT = ([Archetype], [Maybe Position], [Maybe Angle], [Maybe Speed], [Maybe PheremoneAmount])

type EntityT = (Archetype, Maybe Position, Maybe Angle, Maybe Speed, Maybe PheremoneAmount)

-- If this function is used in initWorld, then we should be type safe.
-- Just need much longer zips and unzips!
zipUnzip :: WorldT -> WorldT
zipUnzip (a, b, c, d, e) =
    let entities = (zip5 a b c d e :: [EntityT]) -- Force type checking
    in unzip5 entities



initWorld :: World
initWorld =
    let world = World [] [] [] [] []
        entities = [newBlackAnt (0, 0) (pi/4) 3, newPheremoneDrop (10, 10) 5]
    in appendToWorld entities world

appendToWorld :: [Entity] -> World -> World
appendToWorld entities world = World {
    wTypes     = wTypes world ++ map eType entities,
    wPositions = wPositions world ++ map ePosition entities,
    wAngles    = wAngles world ++ map eAngle entities,
    wSpeeds    = wSpeeds world ++ map eSpeed entities,
    wPheremoneAmount = wPheremoneAmount world ++ map ePheremoneAmount entities
}

newBlackAnt :: Position -> Angle -> Speed -> Entity
newBlackAnt pos angle speed = Entity {
    eType     = BlackAnt,
    ePosition = Just pos,
    eAngle    = Just angle,
    eSpeed    = Just speed,
    ePheremoneAmount = Just 0
}

newPheremoneDrop :: Position -> PheremoneAmount -> Entity
newPheremoneDrop pos amount = Entity {
    eType     = PheremoneDrop,
    ePosition = Just pos,
    eAngle    = Nothing,
    eSpeed    = Nothing,
    ePheremoneAmount = Just amount
}



calcMovement :: Position -> Angle -> Speed -> Position
calcMovement (x, y) angle speed = (x + speed * cos angle, y + speed * sin angle)

updatePositions :: [Maybe Position]
               -> [Maybe Angle]
               -> [Maybe Speed]
               -> [Maybe Position]
updatePositions =
    zipWith3 (\pos angle speed -> case (pos, angle, speed) of
        (Just p, Just a, Just s) -> Just (calcMovement p a s)
        _                        -> pos)

updateWorld :: World -> World
updateWorld world@(World types positions angles speeds pheromoneAmounts) =
    world { wPositions = updatePositions positions angles speeds}

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

