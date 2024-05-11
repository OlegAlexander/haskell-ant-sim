{-# HLINT ignore "Eta reduce" #-}

module Common where

import Control.Monad (replicateM)
import Control.Monad.Trans.State
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


data WheelPos = TurnLeft | Center | TurnRight deriving (Eq, Show)


data AntSprite = LeftSprite | RightSprite deriving (Eq, Show)


type RngSeed = Int


type Position = Vector2


type Angle = Float


type Speed = Float


-- --------------------------------- Entity --------------------------------- --

data Entity = Entity
    { eType :: EntityType,
      ePos :: Position,
      eAngle :: Angle,
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

mkPlayerAnt :: Position -> RngSeed -> Entity
mkPlayerAnt pos seed =
    defaultEntity{eType = PlayerAnt, ePos = pos, eRng = mkStdGen seed}


mkWorld :: [RngSeed] -> World
mkWorld seeds =
    let (playerAntSeed, antSeeds) = case seeds of
            [] -> error "mkWorld: empty seed list"
            (x : xs) -> (x, xs)
    in  V.singleton $ mkPlayerAnt (Vector2 0 0) playerAntSeed


-- ---------------------------- State Monad Test ---------------------------- --

data WorldState = WorldState {nextEntityId :: Int, entities :: [Int]} deriving (Eq, Show)


newEntity :: State WorldState ()
newEntity = do
    n <- gets nextEntityId
    es <- gets entities
    modify (\s -> s{nextEntityId = n + 1, entities = es ++ [n]})


mkWorldState :: Int -> WorldState
mkWorldState n = execState (replicateM n newEntity) (WorldState 1 [])


tick :: State Int Int
tick = do
    n <- get
    put (n + 1)
    return n


plusOne :: Int -> Int
plusOne n = execState tick n


getNextN :: Int -> [Int]
getNextN n = evalState (replicateM n tick) 0