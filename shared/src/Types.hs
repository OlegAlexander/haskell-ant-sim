module Types where

import Raylib.Types (Rectangle, Texture, Vector2)
import Raylib.Util (WindowResources)
import System.Random (StdGen)


data Container = Container
    { containerAmount :: Int,
      containerRect :: Rectangle
    }
    deriving (Eq, Show)


newtype Food = Food {foodContainer :: Container} deriving (Eq, Show)
newtype Nest = Nest {nestContainer :: Container} deriving (Eq, Show)
newtype Pheromone = Pheromone {pheromoneContainer :: Container} deriving (Eq, Show)


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
    { rayPos :: Vector2,
      rayAngle :: Degrees,
      rayLength :: Float,
      rayHitEntityType :: EntityType
    }
    deriving (Eq, Show)


data Ant = Ant
    { antPos :: Vector2,
      antAngle :: Degrees,
      antSpeed :: Float,
      antMode :: Mode,
      antRng :: StdGen,
      antGoDir :: GoDir,
      antWheelPos :: WheelPos,
      antSprite :: Sprite,
      antVisionRays :: [VisionRay],
      antNestAngle :: Degrees,
      antNestDistance :: Float,
      antHasFood :: Bool,
      antScore :: Float
    }
    deriving (Eq, Show)


data Mode = SeekFood | SeekNest deriving (Eq, Show)


data WheelPos = TurnLeft | Center | TurnRight deriving (Eq, Show)


data GoDir = Forward | Stop | Backward deriving (Eq, Show)


data Sprite = LeftSprite | RightSprite deriving (Eq, Show)


data WallDrawingState = Idle | Started | InProgress | Finished
    deriving (Eq, Show)


data World = World
    { wWindowResources :: WindowResources,
      wAntTexture :: Texture,
      wPlayerAnt :: Ant,
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
