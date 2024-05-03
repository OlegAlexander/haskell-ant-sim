-- Short stack trace: cabal run --enable-profiling --ghc-options="-prof -fprof-auto"
--  Long stack trace: cabal run --enable-profiling --ghc-options="-prof -fprof-auto" haskell-ant-sim -- +RTS -xc
-- Disable profiling: cabal run --disable-profiling

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Main where


import           Data.Function                      ((&))
import qualified Data.Vector                        as V
import           Debug.Trace                        (traceShow)
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game

-- ----------------------------------- ECS ---------------------------------- --

type Position = (Float, Float)
type Angle = Float -- in radians
type Speed = Float
type PheremoneAmount = Float

data EntityType = BlackAnt | PheremoneDrop deriving (Eq, Show)

data Entity = Entity {
    eEntityType      :: EntityType,
    ePosition        :: Maybe Position,
    eAngle           :: Maybe Angle,
    eSpeed           :: Maybe Speed,
    ePheremoneAmount :: Maybe PheremoneAmount
} deriving (Eq, Show)


type World = V.Vector Entity

initWorld :: World
initWorld = V.fromList [newBlackAnt (0, 0) (pi/4) 3, newPheremoneDrop (10, 10) 5]


newBlackAnt :: Position -> Angle -> Speed -> Entity
newBlackAnt pos angle speed = Entity {
    eEntityType      = BlackAnt,
    ePosition        = Just pos,
    eAngle           = Just angle,
    eSpeed           = Just speed,
    ePheremoneAmount = Nothing
}

newPheremoneDrop :: Position -> PheremoneAmount -> Entity
newPheremoneDrop pos amount = Entity {
    eEntityType      = PheremoneDrop,
    ePosition        = Just pos,
    eAngle           = Nothing,
    eSpeed           = Nothing,
    ePheremoneAmount = Just amount
}

calcMovement :: Position -> Angle -> Speed -> Position
calcMovement (x, y) angle speed = (x + speed * cos angle, y + speed * sin angle)

movementSystem :: Entity -> Entity
movementSystem e =
    -- Notice how we pull out only the fields we need
    let position = ePosition e
        angle = eAngle e
        speed = eSpeed e
    in case (position, angle, speed) of
        (Just p, Just a, Just s) -> e { ePosition = Just (calcMovement p a s) }
        _                        -> e

-- General pattern: V.map (systemC . systemB . systemA) world
updateWorld :: World -> World
updateWorld world = V.map movementSystem world

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
         (greyN 0.8) fps initWorld makePictures handleEvent stepWorld


makePictures :: World -> Picture
makePictures world = V.map makePicture world & V.toList & pictures

makePicture :: Entity -> Picture
makePicture e =
    let eType = eEntityType e
        position = ePosition e
    in antToPicture eType position
    where
        antToPicture :: EntityType -> Maybe Position -> Picture
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

