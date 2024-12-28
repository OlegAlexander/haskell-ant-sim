{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Shared where

import Control.Monad (unless, (>=>))
import Data.Function ((&))
import Data.Sequence qualified as Seq
import Raylib.Types (Color (..), Rectangle (..), Vector2 (..))
import Raylib.Util.Math (deg2Rad)
import System.Random (mkStdGen, randomR)
import Types (Ant (..), GoDir (..), Sprite (..), WheelPos (..))


(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) p q x = p x || q x


(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) p q x = p x && q x


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


mkAnt :: Vector2 -> Int -> Ant
mkAnt pos seed =
    let (randomAngle, rng) = mkStdGen seed & randomR (0, 360)
        (randomNoise, rng') = rng & randomR (0, 1)
    in  Ant pos randomAngle 0 rng' Stop Center LeftSprite Seq.empty 0 0 False 0 0 randomNoise


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