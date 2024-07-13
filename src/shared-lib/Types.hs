module Types where

import Data.IntMap.Strict (IntMap)
import Raylib.Types (Rectangle, Texture, Vector2)
import Raylib.Util (WindowResources)
import System.Random (StdGen)


data Circle = Circle
    { circlePos :: Vector2,
      circleRadius :: Float
    }
    deriving (Eq, Show)


data VisionRay = VisionRay
    { rayPos :: Vector2,
      rayAngle :: Float, -- in degrees
      rayLength :: Float
    }
    deriving (Eq, Show)


data PlayerAnt = PlayerAnt
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
    = PlayerAntE PlayerAnt
    | AntE
    | DeadAntE
    | PheromoneE Circle
    | FoodE Circle
    | NestE Circle
    deriving (Eq, Show)


instance Ord Entity where
    compare :: Entity -> Entity -> Ordering
    compare e1 e2 = compare (drawOrder e1) (drawOrder e2)
        where
            drawOrder :: Entity -> Int
            drawOrder = \case
                PlayerAntE _ -> 7
                AntE -> 6
                DeadAntE -> 5
                PheromoneE _ -> 4
                FoodE _ -> 3
                NestE _ -> 2


data Mode = SeekFood | SeekNest deriving (Eq, Show)


data WheelPos = TurnLeft | Center | TurnRight deriving (Eq, Show)


data Sprite = LeftSprite | RightSprite deriving (Eq, Show)


data WallDrawingState = Idle | Started | InProgress | Finished
    deriving (Eq, Show)


data World = World
    { wWindowResources :: WindowResources,
      wAntTexture :: Texture,
      wEntities :: [Entity],
      wRenderVisionRays :: Bool,
      wWalls :: [Rectangle],
      wWallBeingDrawn :: Maybe (Vector2, Vector2),
      wVisionRays :: IntMap VisionRay
    }
