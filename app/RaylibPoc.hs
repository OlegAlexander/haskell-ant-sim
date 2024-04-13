{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Main where

import           Ant
import           Control.Monad
import           Data.Function        ((&))
import           Debug.Trace          (traceShow)
import           Raylib.Core
import           Raylib.Core.Shapes
import           Raylib.Core.Text
import           Raylib.Core.Textures
import           Raylib.Types
import           Raylib.Util
import           Raylib.Util.Colors
import           Raylib.Util.Math
import           System.Random        (newStdGen, random)

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
antScale = 0.5

antMaxSpeed :: Float
antMaxSpeed = 5

antStepSize :: Float
antStepSize = 3



drawTextureCentered :: Texture -> Rectangle -> Float -> Float -> Vector2 -> Color -> IO ()
drawTextureCentered texture source@(Rectangle _ _ w h) scale angle (Vector2 x y) color = do
    let w' = w * scale
        h' = h * scale
    drawTexturePro
        texture
        source
        (Rectangle x y w' h')
        (Vector2 (w' / 2) (h' / 2)) -- center of the scaled rect
        angle
        color


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
    & driveAnt antStepSize 0.33 0.33 antMaxSpeed (pi/15) (pi/60)
    & cycleAntSprite antMaxSpeed
    & wrapAroundAntRaylib (fromIntegral screenWidth) (fromIntegral screenHeight)

drawWorld :: Texture -> Ant -> IO ()
drawWorld antTexture ant = do
    hideCursor

    f11Pressed <- isKeyPressed KeyF11
    when f11Pressed toggleFullscreen

    drawing $ do
        let texW = texture'width antTexture
            texH = texture'height antTexture
        clearBackground lightGray
        drawFPS 10 10
        drawTextureCentered
            antTexture
            (Rectangle 0 0 (fromIntegral texW/2) (fromIntegral texH))
            antScale
            (antTheta ant * rad2Deg)
            (Vector2 (antX ant) (antY ant))
            white



gameLoop :: (Texture, Ant) -> IO ()
gameLoop (antTexture, ant) = do
    ant' <- handleInput ant
    let movedAnt = updateWorld ant'
    drawWorld antTexture movedAnt
    shouldClose <- windowShouldClose
    unless shouldClose $ gameLoop (antTexture, movedAnt)

main :: IO ()
main = do
    ant <- initWorld
    window <- initWindow screenWidth screenHeight title
    setTargetFPS 60
    antTexture <- loadTexture "ant.png" window
    gameLoop (antTexture, ant)

