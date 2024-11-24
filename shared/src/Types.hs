-- Use NoFieldSelectors to force the use of OverloadedRecordDot everywhere.
{-# LANGUAGE NoFieldSelectors #-}

module Types where

import Raylib.Types (Color, Rectangle, Vector2)
import System.Random (StdGen)


data Container = Container
    { cAmount :: Int,
      cRect :: Rectangle
    }
    deriving (Eq, Show)


newtype Food = Food {fContainer :: Container} deriving (Eq, Show)
newtype Nest = Nest {nContainer :: Container} deriving (Eq, Show)
newtype Pheromone = Pheromone {pContainer :: Container} deriving (Eq, Show)


data EntityType
    = PlayerAntET
    | AntET
    | DeadAntET
    | PheromoneET
    | FoodET
    | NestET
    | WallET
    | UnknownET
    deriving (Eq, Show)


type Degrees = Float


data VisionRay = VisionRay
    { rPos :: Vector2,
      rAngle :: Degrees,
      rLength :: Float,
      rHitEntityType :: EntityType,
      rColor :: Color
    }
    deriving (Eq, Show)


data Ant = Ant
    { aPos :: Vector2,
      aAngle :: Degrees,
      aSpeed :: Float,
      aRng :: StdGen,
      aGoDir :: GoDir,
      aWheelPos :: WheelPos,
      aSprite :: Sprite,
      aVisionRays :: [VisionRay],
      aNestAngle :: Float, -- Normalized degrees
      aNestDistance :: Float, -- Normalized distance
      aHasFood :: Bool,
      aScore :: Float,
      aRegeneratePheromoneCounter :: Int
    }
    deriving (Show) -- No Eq because of StdGen 1.1


data WheelPos = TurnLeft | Center | TurnRight deriving (Eq, Show)


data GoDir = Forward | Stop | Backward deriving (Eq, Show)


data Sprite = LeftSprite | RightSprite deriving (Eq, Show)


data WallDrawingState = Idle | Started | InProgress | Finished
    deriving (Eq, Show)


data World = World
    { wPlayerAnt :: Ant,
      wAnts :: [Ant],
      wNest :: Nest,
      wRenderVisionRays :: Bool,
      wRenderVisionRects :: Bool,
      wRenderHomeVector :: Bool,
      wRenderHomeCompass :: Bool,
      wWalls :: [Rectangle],
      wWallBeingDrawn :: Maybe (Vector2, Vector2),
      wFood :: [Food],
      wFoodBeingDrawn :: Maybe Food,
      wPheromones :: [Pheromone]
    }


data AntDecision
    = GoLeft
    | GoForwardLeft
    | GoForward
    | GoForwardRight
    | GoRight
    | GoBackwardRight
    | GoBackward
    | GoBackwardLeft
    | GoNowhere
    deriving (Enum, Eq, Show)
