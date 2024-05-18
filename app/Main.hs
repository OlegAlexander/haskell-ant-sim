{-# HLINT ignore "Eta reduce" #-}

module Main where

import Ant
import Control.Monad (unless, when)
import Data.Function ((&))
import Debug.Trace (traceShow)
import GHC.Float (int2Float)
import Raylib.Core
import Raylib.Core.Shapes
import Raylib.Core.Text
import Raylib.Core.Textures
import Raylib.Types
import Raylib.Util
import Raylib.Util.Colors
import Raylib.Util.Math
import System.Random (randomIO)


screenWidth :: Int
screenWidth = 1920


screenHeight :: Int
screenHeight = 1080


screenCenterW :: Float
screenCenterW = int2Float screenWidth / 2


screenCenterH :: Float
screenCenterH = int2Float screenHeight / 2


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


antPng :: String
antPng = "assets/ant.png"


type Exit = Bool


-- TODO Recommend this function to Raylib author
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


initWorld :: IO (Texture, Ant, Exit)
initWorld = do
    seed <- randomIO
    let ant = mkAnt screenCenterW screenCenterH seed
    window <- initWindow screenWidth screenHeight title
    setTargetFPS 60
    antTexture <- loadTexture antPng window
    return (antTexture, ant, False)


handleInput :: (Texture, Ant, Exit) -> IO (Texture, Ant, Exit)
handleInput (tex, ant, exit) = do
    go <- isKeyDown KeyUp
    stop <- isKeyDown KeyDown
    left <- isKeyDown KeyLeft
    right <- isKeyDown KeyRight
    let ant' =
            ant
                { antStopGo = if go then Go else if stop then Stop else Neutral,
                  antWheelPos = if left then TurnLeft else if right then TurnRight else Center
                }
    exit' <- windowShouldClose
    return (tex, ant', exit')


updateWorld :: (Texture, Ant, Exit) -> (Texture, Ant, Exit)
updateWorld (antTexture, ant, exit) =
    let ant' =
            ant
                & driveAnt antStepSize 0.33 0.33 antMaxSpeed (pi / 15) (pi / 60)
                & cycleAntSprite antMaxSpeed
                & wrapAroundAntRaylib (int2Float screenWidth) (int2Float screenHeight)
    in  (antTexture, ant', exit)


renderWorld :: (Texture, Ant, Exit) -> IO ()
renderWorld (antTexture, ant, exit) = do
    hideCursor

    f11Pressed <- isKeyPressed KeyF11
    when f11Pressed toggleFullscreen

    drawing $ do
        let texW = texture'width antTexture
            texH = texture'height antTexture
            sprite = antSprite ant
            spriteRect = case sprite of
                LeftSprite -> Rectangle 0 0 (int2Float texW / 2) (int2Float texH)
                RightSprite -> Rectangle (int2Float texW / 2) 0 (int2Float texW / 2) (int2Float texH)
        clearBackground lightGray
        drawFPS 10 10
        drawTextureCentered
            antTexture
            spriteRect
            antScale
            (antTheta ant * rad2Deg)
            (Vector2 (antX ant) (antY ant))
            white


shouldExit :: (Texture, Ant, Exit) -> Bool
shouldExit (_, _, exit) = exit


-- A generic game loop!
gameLoop :: (w -> IO w) -> (w -> w) -> (w -> IO ()) -> (w -> Bool) -> w -> IO ()
gameLoop handleInputFunc updateFunc renderFunc shouldExitFunc world = do
    world' <- handleInputFunc world
    unless (shouldExitFunc world') $ do
        let world'' = updateFunc world'
        renderFunc world''
        gameLoop handleInputFunc updateFunc renderFunc shouldExitFunc world''


main :: IO ()
main = initWorld >>= gameLoop handleInput updateWorld renderWorld shouldExit
