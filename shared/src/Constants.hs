module Constants where

import Raylib.Types (Color)
import Raylib.Util.Colors (blue, white, yellow)


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
antAcceleration = 0.5


antTurnAngle :: Float
antTurnAngle = 5


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


foodScale :: Float
foodScale = 1.0


foodColor :: Color
foodColor = yellow


foodGrowthAmount :: Int
foodGrowthAmount = 1


collisionRectSize :: Float
collisionRectSize = 50


nestSize :: Float
nestSize = 25


pheromoneColor :: Color
pheromoneColor = blue


pheromoneSize :: Float
pheromoneSize = 10


pheromoneScale :: Float
pheromoneScale = 0.01


initPheromoneAmount :: Int
initPheromoneAmount = 1000


regeneratePheromoneDelay :: Int
regeneratePheromoneDelay = 20