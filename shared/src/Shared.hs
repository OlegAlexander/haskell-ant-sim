{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Shared where

import Constants (antMaxSpeed, hitboxSize, nnParameterRange, screenHeight, screenWidth, regeneratePheromoneDelayMax, regeneratePheromoneDelayMin, fenceWallThickness)
import Control.Monad (forM_, unless, when, (>=>))
import Data.Function ((&))
import Data.List (mapAccumL)
import Data.Sequence qualified as Seq
import Data.Tuple (swap)
import GHC.Float (int2Float)
import NeuralNetwork (initFlatLayers, unflattenLayers)
import Raylib.Core (takeScreenshot)
import Raylib.Core.Text (drawText)
import Raylib.Types (Color (..), Rectangle (..), Vector2 (..))
import Raylib.Util.Colors (darkBrown)
import Raylib.Util.Math (deg2Rad)
import System.Random (StdGen, mkStdGen, random, randomR)
import Types (
    Ant (..),
    Container (..),
    GoDir (..),
    Nest (..),
    Sprite (..),
    TrainingMode (..),
    WheelPos (..),
    World (..),
 )
import ScreenshotOps (fixScreenshot, diffScreenshots)


(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) p q x = p x || q x


(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) p q x = p x && q x


-- A version of mapAccumL that takes a function with the return tuple values swapped
-- and returns a tuple with the values swapped.
mapAccumL' :: (Traversable t) => (acc -> a -> (b, acc)) -> acc -> t a -> (t b, acc)
mapAccumL' f acc xs = swap (mapAccumL (\a x -> swap (f a x)) acc xs)


rgbToLinear :: Color -> (Float, Float, Float, Float)
rgbToLinear (Color r g b a) =
    let r' = fromIntegral r / 255
        g' = fromIntegral g / 255
        b' = fromIntegral b / 255
        a' = fromIntegral a / 255
    in  (r', g', b', a')


linearToRgb :: (Float, Float, Float, Float) -> Color
linearToRgb (r, g, b, a) =
    let r' = round $ r * 255
        g' = round $ g * 255
        b' = round $ b * 255
        a' = round $ a * 255
    in  Color r' g' b' a'


scalarTimesColor :: Float -> Color -> Color
scalarTimesColor scalar color =
    let (r', g', b', a') = rgbToLinear color
    in  linearToRgb (r' * scalar, g' * scalar, b' * scalar, a')


getNextPos :: Float -> Float -> Vector2 -> Vector2
getNextPos angle speed (Vector2 x y) =
    let rad = (-angle) * deg2Rad -- negate angle because of screen space coords
        x' = x + speed * cos rad
        y' = y + speed * sin rad
    in  Vector2 x' y'


calcCenteredRect :: Vector2 -> Float -> Rectangle
calcCenteredRect (Vector2 x y) size =
    Rectangle (x - size / 2) (y - size / 2) size size


calcRectCenter :: Rectangle -> Vector2
calcRectCenter (Rectangle x y w h) = Vector2 (x + w / 2) (y + h / 2)


isPointInRect :: Vector2 -> Rectangle -> Bool
isPointInRect (Vector2 x y) (Rectangle rx ry rw rh) =
    x > rx && x < rx + rw && y > ry && y < ry + rh


-- Normalize a value to the range [0, 1]
normalize :: Float -> Float -> Float
normalize x maxVal = if x >= 0 && maxVal > 0 then min 1.0 (x / maxVal) else 0


drawTextLines :: Int -> Int -> Int -> Int -> Color -> [String] -> IO ()
drawTextLines x y verticalOffset fontSize color stats = do
    forM_ (zip [0 ..] stats) $ \(i, line) ->
        drawText line x (y + i * verticalOffset) fontSize color


drawTextLines' :: [String] -> IO ()
drawTextLines' = drawTextLines 10 10 40 30 darkBrown


mkAnt :: StdGen -> Vector2 -> (Ant, StdGen)
mkAnt rng pos =
    let (!randomAngle, !rng') = randomR (0, 360) rng
        (!regeneratePheromoneDelay, !rng'') =  randomR (regeneratePheromoneDelayMin, regeneratePheromoneDelayMax) rng'
        (!flatNeuralNetworkForaging, !rng''') = initFlatLayers [99, 99, 6] 0.1 rng''
        (!flatNeuralNetworkReturning, !rng'''') = initFlatLayers [99, 99, 6] 0.1 rng'''
    in  ( Ant
            { aPos = pos,
              aAngle = randomAngle,
              aSpeed = 0,
              aMaxSpeed = antMaxSpeed,
              aGoDir = Stop,
              aWheelPos = Center,
              aSprite = LeftSprite,
              aVisionRays = Seq.empty,
              aNestAngle = 0,
              aNestAntAngleDelta = 0,
              aNestDistance = 0,
              aNestDistanceWhenFoodPickedUp = 0,
              aHasFood = False,
              aScore = 0,
              aPheromoneCounter = 0,
              aRegeneratePheromoneDelay = regeneratePheromoneDelay,
              aRegeneratePheromoneCounter = 0,
              aForagingBrain = unflattenLayers flatNeuralNetworkForaging,
              aReturningBrain = unflattenLayers flatNeuralNetworkReturning
            },
          rng''''
        )


fenceWalls :: Float -> Seq.Seq Rectangle
fenceWalls thickness =
    Seq.fromList
        [ Rectangle 0 0 (int2Float screenWidth) thickness, -- Top
          Rectangle 0 (int2Float screenHeight - thickness) (int2Float screenWidth) thickness, -- Bottom
          Rectangle 0 0 thickness (int2Float screenHeight), -- Left
          Rectangle (int2Float screenWidth - thickness) 0 thickness (int2Float screenHeight) -- Right
        ]


defaultWorld :: StdGen -> World
defaultWorld rng =
    let screenCenter = Vector2 (int2Float screenWidth / 2) (int2Float screenHeight / 2)
        (!playerAnt, !rng') = mkAnt rng screenCenter
        nest = Nest (Container 0 (calcCenteredRect screenCenter hitboxSize))
    in  World
            { wPlayerAnt = playerAnt,
              wAnts = Seq.empty,
              wNest = nest,
              wRenderVisionRays = False,
              wRenderVisionRects = False,
              wRenderHomeVector = False,
              wRenderHomeCompass = False,
              wRenderNNVectors = False,
              wRenderDebugText = False,
              wWalls = fenceWalls fenceWallThickness,
              wWallBeingDrawn = Nothing,
              wFood = Seq.empty,
              wFoodBeingDrawn = Nothing,
              wPheromones = Seq.empty,
              wTrainingMode = Off,
            --   wTrainingMode = Fast,
              wTicks = 0,
              wGeneration = 0,
              wCourse = 0,
              wBestAvgScore = 0,
              wRng = rng'
            }


data System w = System
    { handleInput :: w -> IO w,
      update :: w -> w,
      render :: w -> IO ()
    }


instance Semigroup (System w) where
    (<>) :: System w -> System w -> System w
    (<>) sys1 sys2 =
        System
            { handleInput = sys1.handleInput >=> sys2.handleInput,
              update = sys2.update . sys1.update,
              render = \w -> sys1.render w >> sys2.render w
            }


instance Monoid (System w) where
    mempty :: System w
    mempty = System return id (\_ -> return ())


gameLoop :: System w -> IO Bool -> w -> IO ()
gameLoop sys shouldExitFunc world = do
    shouldExit <- shouldExitFunc
    unless shouldExit $ do
        world' <- sys.handleInput world
        let world'' = sys.update world'
        sys.render world''
        gameLoop sys shouldExitFunc world''