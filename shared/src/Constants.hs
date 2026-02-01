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
numAnts = 100


antScale :: Float
antScale = 0.3


antMaxSpeed :: Float
antMaxSpeed = 10


antAcceleration :: Float
antAcceleration = 0.3


antTurnAngle :: Float
antTurnAngle = 5


antJitterAngle :: Float
antJitterAngle = 5


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


fenceWallThickness :: Float
fenceWallThickness = 10


foodScale :: Float
foodScale = 0.01


foodColor :: Color
foodColor = yellow


foodGrowthAmount :: Int
foodGrowthAmount = 100


hitboxSize :: Float
hitboxSize = 60


nestSize :: Float
nestSize = hitboxSize / 2


nestColor :: Color
nestColor = maroon


pheromoneColor :: Color
pheromoneColor = orange


pheromoneScale :: Float
pheromoneScale = 0.04


initPheromoneAmount :: Int
initPheromoneAmount = 300


regeneratePheromoneDelayMin :: Int
regeneratePheromoneDelayMin = 10


regeneratePheromoneDelayMax :: Int
regeneratePheromoneDelayMax = 100


maxPheromonesPerFood :: Int
maxPheromonesPerFood = 1


maxPheromones :: Int
maxPheromones = 300


ticksPerCourse :: Int
ticksPerCourse = fps * 30


coursesPerGeneration :: Int
coursesPerGeneration = 3


maxGenerations :: Int
maxGenerations = 10000


mutationRate :: Float
mutationRate = 0.03


nnParameterRange :: Float
nnParameterRange = 10.0


outForagingBrainFile :: FilePath
outForagingBrainFile = "models/foraging.brain"


outReturningBrainFile :: FilePath
outReturningBrainFile = "models/returning.brain"


inForagingBrainFile :: FilePath
inForagingBrainFile = "models/foraging_2000_7.42_99_99_6_presentation_6cd6084.brain"


inReturningBrainFile :: FilePath
inReturningBrainFile = "models/returning_2000_7.42_99_99_6_presentation_6cd6084.brain"


usePretrainedBrains :: Bool
usePretrainedBrains = True