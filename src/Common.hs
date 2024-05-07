module Common where

import Data.Function ((&))
import Data.Vector qualified as V
import Raylib.Types
import System.Random (StdGen, mkStdGen)


-- ------------------------------- Components ------------------------------- --

data EntityType
    = PlayerAnt
    | Ant
    | DeadAnt
    | Pheromone
    | Food
    | Nest
    | Wall
    deriving (Eq, Show)


data AntMode = SeekFood | SeekNest deriving (Eq, Show)


data PedalPos = Decelerate | Neutral | Accelerate deriving (Eq, Show)


data WheelPos = Left | Center | Right deriving (Eq, Show)


data AntSprite = LeftSprite | RightSprite deriving (Eq, Show)


type RngSeed = Int


type Pos = Vector2


type AngleInDegrees = Float


type Speed = Float


-- --------------------------------- Entity --------------------------------- --

data Entity = Entity
    { eType :: EntityType,
      ePos :: Pos,
      eAngle :: AngleInDegrees,
      eSpeed :: Speed,
      eMode :: AntMode,
      eRng :: StdGen,
      eStopGo :: PedalPos,
      eWheel :: WheelPos,
      eSprite :: AntSprite
    }
    deriving (Eq, Show)


defaultEntity :: Entity
defaultEntity =
    Entity
        { eType = Ant,
          ePos = Vector2 0 0,
          eAngle = 0,
          eSpeed = 0,
          eMode = SeekFood,
          eRng = undefined,
          eStopGo = Neutral,
          eWheel = Center,
          eSprite = LeftSprite
        }


type World = V.Vector Entity


-- --------------------------- Entity Constructors -------------------------- --

mkPlayerAnt :: Pos -> RngSeed -> Entity
mkPlayerAnt pos seed =
    defaultEntity {eType = PlayerAnt, ePos = pos, eRng = mkStdGen seed}


mkWorld :: [RngSeed] -> World
mkWorld seeds =
    let (playerAntSeed, antSeeds) = case seeds of
            [] -> error "mkWorld: empty seed list"
            (x : xs) -> (x, xs)
    in  V.singleton $ mkPlayerAnt (Vector2 0 0) playerAntSeed
