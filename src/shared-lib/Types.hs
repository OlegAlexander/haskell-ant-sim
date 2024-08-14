module Types where

import Raylib.Types (Rectangle, Texture, Vector2)
import Raylib.Util (WindowResources)
import System.Random (StdGen)


data Circle = Circle
    { circlePos :: Vector2,
      circleRadius :: Float
    }
    deriving (Eq, Show)


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


data VisionRay = VisionRay
    { rayPos :: Vector2,
      rayAngle :: Float, -- in degrees
      rayLength :: Float,
      rayHitEntityType :: EntityType
    }
    deriving (Eq, Show)


data Ant = Ant
    { antPos :: Vector2,
      antAngle :: Float, -- in degrees
      antSpeed :: Float,
      antMode :: Mode,
      antRng :: StdGen,
      antGo :: Bool,
      antWheelPos :: WheelPos,
      antSprite :: Sprite,
      antVisionRays :: [VisionRay]
    }
    deriving (Eq, Show)


data Entity
    = PlayerAntE Ant
    | AntE
    | DeadAntE
    | PheromoneE Circle
    | FoodE Circle
    | NestE Circle
    deriving (Eq, Show)


data Mode = SeekFood | SeekNest deriving (Eq, Show)


data WheelPos = TurnLeft | Center | TurnRight deriving (Eq, Show)


data Sprite = LeftSprite | RightSprite deriving (Eq, Show)


data WallDrawingState = Idle | Started | InProgress | Finished
    deriving (Eq, Show)


data World = World
    { wWindowResources :: WindowResources,
      wAntTexture :: Texture,
      wPlayerAnt :: Ant,
      wRenderVisionRays :: Bool,
      wRenderVisionRects :: Bool,
      wWalls :: [Rectangle],
      wWallBeingDrawn :: Maybe (Vector2, Vector2)
    }
