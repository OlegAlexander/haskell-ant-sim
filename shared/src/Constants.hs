module Constants where

import Raylib.Types (Color)
import Raylib.Util.Colors (gray, maroon, orange, white, yellow)


screenWidth :: Int
screenWidth = 1920


screenHeight :: Int
screenHeight = 1080


bgColor :: Color
bgColor = gray


fps :: Int
fps = 60


numAnts :: Int
numAnts = 40


antScale :: Float
antScale = 0.3


antMaxSpeed :: Float
antMaxSpeed = 8


antAcceleration :: Float
antAcceleration = 0.5


antTurnAngle :: Float
antTurnAngle = 5


antJitterAngle :: Float
antJitterAngle = 3


antVisionAngle :: Float
antVisionAngle = 135


antVisionMaxDistance :: Float
antVisionMaxDistance = 500


antVisionResolution :: Int
antVisionResolution = 32


compassMaxDistance :: Float
compassMaxDistance = sqrt (fromIntegral screenWidth ** 2 + fromIntegral screenHeight ** 2) / 2


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
nestSize = 20


nestColor :: Color
nestColor = maroon


pheromoneColor :: Color
pheromoneColor = orange


pheromoneSize :: Float
pheromoneSize = 10


pheromoneScale :: Float
pheromoneScale = 0.05


initPheromoneAmount :: Int
initPheromoneAmount = 300


regeneratePheromoneDelay :: Int
regeneratePheromoneDelay = 20


maxPheromones :: Int
maxPheromones = 50


ticksPerGeneration :: Int
ticksPerGeneration = fps * 30


maxGenerations :: Int
maxGenerations = 10000


mutationRate :: Float
mutationRate = 0.01