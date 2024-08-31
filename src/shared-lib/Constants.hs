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
antMaxSpeed = 8


antAcceleration :: Float
antAcceleration = 0.333


antTurnAngle :: Float
antTurnAngle = 3


antJitterAngle :: Float
antJitterAngle = 1


antVisionAngle :: Float
antVisionAngle = 180


antVisionMaxDistance :: Float
antVisionMaxDistance = 500


antVisionResolution :: Int
antVisionResolution = 64


antPng :: String
antPng = "assets/ant.png"


wallColor :: Color
wallColor = white


minWallSize :: Float
minWallSize = 10.0


borderWallThickness :: Float
borderWallThickness = 30