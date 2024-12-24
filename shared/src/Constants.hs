module Constants where

import Raylib.Types (Color)
import Raylib.Util.Colors (blue, brown, white, yellow)


screenWidth :: Int
screenWidth = 1920


screenHeight :: Int
screenHeight = 1080


fps :: Int
fps = 60


numAnts :: Int
numAnts = 100


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
antVisionAngle = 135


antVisionMaxDistance :: Float
antVisionMaxDistance = 500


antVisionResolution :: Int
antVisionResolution = 32


compassMaxDistance :: Float
compassMaxDistance = 1000


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
collisionRectSize = 30


nestSize :: Float
nestSize = 25


nestColor :: Color
nestColor = brown


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


maxPheromones :: Int
maxPheromones = 50