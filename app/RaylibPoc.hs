{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Main where

import           Ant
import           Control.Monad
import           Data.Function      ((&))
import           Debug.Trace        (traceShow)
import           Raylib.Core
import           Raylib.Core.Shapes
import           Raylib.Core.Text
import           Raylib.Types
import           Raylib.Util
import           Raylib.Util.Colors
import           Raylib.Util.Math
import           System.Random      (newStdGen, random)

screenWidth :: Int
screenWidth = 1920

screenHeight :: Int
screenHeight = 1080

screenCenterW :: Float
screenCenterW = fromIntegral screenWidth / 2

screenCenterH :: Float
screenCenterH = fromIntegral screenHeight / 2

title :: String
title = "Raylib POC"

fps :: Int
fps = 60

antScale :: Float
antScale = 5

antMaxSpeed :: Float
antMaxSpeed = 5

antStepSize :: Float
antStepSize = 2


initWorld :: IO Ant
initWorld = do
    rng <- newStdGen
    let (seed, _) = random rng
        ant = mkAnt screenCenterW screenCenterH seed
    return ant

handleInput :: Ant -> IO Ant
handleInput ant = do
    go <- isKeyDown KeyUp
    stop <- isKeyDown KeyDown
    left <- isKeyDown KeyLeft
    right <- isKeyDown KeyRight
    let ant' = ant
            { antStopGo = if go then Go else if stop then Stop else Neutral
            , antWheelPos = if left then Ant.Left else if right then Ant.Right else Center
            }
    return ant'

updateWorld :: Ant -> Ant
updateWorld ant = ant
    & driveAnt antStepSize 0.5 0.5 antMaxSpeed (pi/15) (pi/60)
    & cycleAntSprite antMaxSpeed
    & wrapAroundAntRaylib (fromIntegral screenWidth) (fromIntegral screenHeight)

drawWorld :: Ant -> IO ()
drawWorld ant = do
    hideCursor

    f11Pressed <- isKeyPressed KeyF11
    when f11Pressed toggleFullscreen

    drawing $ do
        clearBackground lightGray
        drawFPS 10 10
        drawCircleV (Vector2 (antX ant) (antY ant)) antScale black


gameLoop :: Ant -> IO Ant
gameLoop ant = do
    ant' <- handleInput ant
    let movedAnt = updateWorld ant'
    drawWorld movedAnt
    return movedAnt

main :: IO ()
main = do
    ant <- initWorld
    withWindow screenWidth screenHeight title fps $ const $
        whileWindowOpen_ gameLoop ant

