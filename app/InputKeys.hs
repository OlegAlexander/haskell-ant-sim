-- Source: https://github.com/futu2/h-raylib-examples/blob/master/src/InputKeys.hs

module Main where

import Control.Monad
import Raylib.Core
import Raylib.Core.Shapes
import Raylib.Core.Text
import Raylib.Types
import Raylib.Util
import Raylib.Util.Colors
import Raylib.Util.Math


screenWidth :: Int
screenWidth = 1920


screenHeight :: Int
screenHeight = 1080


title :: String
title = "raylib [core] example - keyboard input"


fps :: Int
fps = 60


stepSize :: Float
stepSize = 10


initBallPosition :: Vector2
initBallPosition =
    Vector2 (fromIntegral screenWidth / 2) (fromIntegral screenHeight / 2)


loop :: Vector2 -> IO Vector2
loop position = do
    changes <-
        vectorSum . fmap snd
            <$> filterM
                (isKeyDown . fst)
                [ (KeyRight, Vector2 stepSize 0),
                  (KeyLeft, Vector2 (-stepSize) 0),
                  (KeyDown, Vector2 0 stepSize),
                  (KeyUp, Vector2 0 (-stepSize))
                ]
    let newPosition = position |+| changes
    drawing $ do
        clearBackground rayWhite
        drawText "move the ball with arrow keys" 10 10 20 darkGray
        drawCircleV newPosition 50 maroon
    return newPosition


main :: IO ()
main =
    withWindow screenWidth screenHeight title fps $
        const $
            void $
                whileWindowOpen loop initBallPosition