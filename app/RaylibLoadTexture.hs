{-# OPTIONS -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Raylib.Core
import           Raylib.Core.Shapes
import           Raylib.Core.Textures
import           Raylib.Types
import           Raylib.Util
import           Raylib.Util.Colors

screenWidth, screenHeight :: Int
screenWidth  = 1920; screenHeight = 1080

startup :: IO (WindowResources, Texture)
startup = do
  window <- initWindow screenWidth screenHeight "loadTexture example"
  setTargetFPS 60
  texture <- loadTexture "ant.png" window
  return (window, texture)

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



mainLoop :: (WindowResources, Texture) -> IO (WindowResources, Texture)
mainLoop (window, texture) = do
  drawing $ do
      let texW = texture'width texture
          texH = texture'height texture
      clearBackground lightGray
      drawTextureCentered texture (Rectangle 0 0 (fromIntegral texW) (fromIntegral texH)) 1 (-90) (Vector2 ((fromIntegral screenWidth / 2)) ((fromIntegral screenHeight / 2))) white
      drawCircleV (Vector2 (fromIntegral screenWidth / 2) (fromIntegral screenHeight / 2)) 5 red
      drawRectangleLines (screenWidth `div` 2 - (texW `div` 2)) (screenHeight `div` 2 - (texH `div` 2)) texW texH red
  return (window, texture)

shouldClose :: (WindowResources, Texture) -> IO Bool
shouldClose _ = windowShouldClose

teardown :: (WindowResources, Texture) -> IO ()
teardown (wr, _) = closeWindow wr

raylibApplication 'startup 'mainLoop 'shouldClose 'teardown
