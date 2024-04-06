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


handleEvents :: Ant -> IO Ant
handleEvents ant = do
    go <- isKeyDown KeyUp
    stop <- isKeyDown KeyDown
    left <- isKeyDown KeyLeft
    right <- isKeyDown KeyRight
    let ant' = ant
            { antStopGo = if go then Go else if stop then Stop else Neutral
            , antWheelPos = if left then Ant.Left else if right then Ant.Right else Center
            }
    return ant'


loop :: Ant -> IO Ant
loop ant = do
    hideCursor

    -- toggle fullscreen
    f11Pressed <- isKeyPressed KeyF11
    when f11Pressed toggleFullscreen

    currentFps <- getFPS
    ant' <- handleEvents ant
    let movedAnt = ant'
                & driveAnt antStepSize 0.5 0.5 antMaxSpeed (pi/15) (pi/60)
                & cycleAntSprite antMaxSpeed
                & wrapAroundAntRaylib (fromIntegral screenWidth) (fromIntegral screenHeight)
    drawing $ do
        clearBackground lightGray
        drawText (show currentFps ++ " fps") 10 10 25 darkGray
        drawCircleV (Vector2 (antX movedAnt) (antY movedAnt)) antScale black
    return movedAnt

main :: IO ()
main = do
    rng <- newStdGen
    let (seed, _) = random rng
        ant = mkAnt screenCenterW screenCenterH seed
    withWindow screenWidth screenHeight title fps $ const $
        whileWindowOpen_ loop ant

