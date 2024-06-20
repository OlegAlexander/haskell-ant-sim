module Constants where

import Raylib.Types (Color)
import Raylib.Util.Colors (white)


screenWidth :: Int
screenWidth = 1920


screenHeight :: Int
screenHeight = 1080


title :: String
title = "Raylib POC"


fps :: Int
fps = 60


antScale :: Float
antScale = 0.3


antMaxSpeed :: Float
antMaxSpeed = 6


antStepSize :: Float
antStepSize = 2


antAcceleration :: Float
antAcceleration = 0.5


antDeceleration :: Float
antDeceleration = 0.5


antTurnAngle :: Float
antTurnAngle = 5


antJitterAngle :: Float
antJitterAngle = 1


antVisionAngle :: Float
antVisionAngle = 90


antVisionMaxDistance :: Float
antVisionMaxDistance = 500


antVisionResolution :: Int
antVisionResolution = 1500


antPng :: String
antPng = "assets/ant.png"


wallColor :: Color
wallColor = white


minWallSize :: Float
minWallSize = 10.0


borderWallThickness :: Float
borderWallThickness = 30