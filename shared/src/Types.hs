-- Use NoFieldSelectors to force the use of OverloadedRecordDot everywhere.
{-# LANGUAGE NoFieldSelectors #-}

module Types where

import Data.Sequence (Seq)
import Raylib.Types (Color, Rectangle, Vector2)
import System.Random (StdGen)
import NeuralNetwork (Layer)


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
      aVisionRays :: Seq VisionRay,
      aNestAngle :: Float, -- Normalized degrees
      aNestDistance :: Float, -- Normalized distance
      aHasFood :: Bool,
      aScore :: Float,
      aRegeneratePheromoneCounter :: Int,
      aRandomNoise :: Float, -- Range [0, 1]
      aBrain :: [Layer]
    }
    deriving (Eq, Show) 


data WheelPos = TurnLeft | Center | TurnRight deriving (Eq, Show)


data GoDir = Forward | Stop | Backward deriving (Eq, Show)


data Sprite = LeftSprite | RightSprite deriving (Eq, Show)


data WallDrawingState = Idle | Started | InProgress | Finished | Deleted
    deriving (Eq, Show)


data World = World
    { wPlayerAnt :: Ant,
      wAnts :: Seq Ant,
      wNest :: Nest,
      wRenderVisionRays :: Bool,
      wRenderVisionRects :: Bool,
      wRenderHomeVector :: Bool,
      wRenderHomeCompass :: Bool,
      wWalls :: Seq Rectangle,
      wWallBeingDrawn :: Maybe (Vector2, Vector2),
      wFood :: Seq Food,
      wFoodBeingDrawn :: Maybe Food,
      wPheromones :: Seq Pheromone
    }
    deriving (Eq, Show)


data AntDecision
    = GoLeft
    | GoForwardLeft
    | GoForward
    | GoForwardRight
    | GoRight
      -- | GoBackwardRight
      -- | GoBackward
      -- | GoBackwardLeft
      -- | GoNowhere
    deriving (Enum, Eq, Show)
