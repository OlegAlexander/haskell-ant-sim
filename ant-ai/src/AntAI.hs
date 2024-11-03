{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use guards" #-}

module AntAI where

import Control.Monad (replicateM, when)
import Data.Fixed (mod')
import Data.Function ((&))
import Data.List (foldl')

-- import Debug.Trace (trace, traceShow)

import AntMovement (antMovementSys)
import Constants (antAcceleration, antJitterAngle, antMaxSpeed, antScale, antTurnAngle, borderWallThickness, collisionRectSize, fps, numAnts, screenHeight, screenWidth)
import DrawWalls (drawWallsSys)
import FlatlandRenderer (flatlandRendererSys)
import Food (foodSys)
import GHC.Float (int2Float)
import Pheromones (pheromoneSys)
import Raylib.Core (
    clearBackground,
    initWindow,
    isKeyDown,
    isKeyPressed,
    setMouseCursor,
    setTargetFPS,
    setTraceLogLevel,
    toggleFullscreen,
    windowShouldClose,
 )
import Raylib.Core.Text (drawFPS)
import Raylib.Types (
    KeyboardKey (..),
    MouseCursor (MouseCursorCrosshair),
 )
import Raylib.Types.Core (Vector2 (..))
import Raylib.Util (drawing)
import Raylib.Util.Colors (lightGray, white)
import Shared (System (..), calcCenteredRect, gameLoop, mkAnt)
import System.Random (mkStdGen, randomIO, randomR)
import Types (
    Ant (..),
    Container (..),
    GoDir (..),
    Mode (SeekFood),
    Nest (..),
    Sprite (LeftSprite, RightSprite),
    WheelPos (Center, TurnLeft, TurnRight),
    World (..),
 )


initWorld :: IO World
initWorld = do
    playerAntSeed <- randomIO
    antSeeds <- replicateM numAnts randomIO
    let antPos' = Vector2 (int2Float screenWidth / 2) (int2Float screenHeight / 2)
        playerAnt = mkAnt antPos' playerAntSeed
        ants = foldl' (\ants' seed -> mkAnt antPos' seed : ants') [] antSeeds
        nest = Nest (Container 0 (calcCenteredRect antPos' collisionRectSize))
        walls = []
    _ <- initWindow screenWidth screenHeight "Ant AI"
    setTargetFPS fps
    setMouseCursor MouseCursorCrosshair
    return $ World playerAnt ants nest True False False True walls Nothing [] Nothing []


antAISys :: System World
antAISys =
    let allSystems =
            drawWallsSys
                <> pheromoneSys
                <> foodSys
                <> antMovementSys
                <> flatlandRendererSys
    in  allSystems
            { render = \w -> drawing $ do
                f11Pressed <- isKeyPressed KeyF11
                when f11Pressed toggleFullscreen
                clearBackground lightGray
                render allSystems w
                drawFPS 10 10
            }


main :: IO ()
main = initWorld >>= gameLoop antAISys windowShouldClose
