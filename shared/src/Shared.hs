module Shared where

import Control.Monad (unless, (>=>))
import Data.Function ((&))
import Raylib.Types (Rectangle (..), Vector2 (..))
import Raylib.Util.Math (deg2Rad)
import System.Random (mkStdGen, randomR)
import Types (Ant (..), GoDir (..), Mode (..), Sprite (..), WheelPos (..))


{- |
Source: https://hackage.haskell.org/package/ilist-0.4.0.1/docs/Data-List-Index.html#v:setAt
'setAt' sets the element at the index.

If the index is negative or exceeds list length, the original list will be returned.
-}
setAt :: Int -> a -> [a] -> [a]
setAt i a ls
    | i < 0 = ls
    | otherwise = go i ls
    where
        go 0 (_ : xs) = a : xs
        go n (x : xs) = x : go (n - 1) xs
        go _ [] = []
{-# INLINE setAt #-}


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
    in  Ant pos randomAngle 0 SeekFood rng Stop Center LeftSprite [] 0 0 False 0 0


data System w = System
    { handleInput :: w -> IO w,
      update :: w -> w,
      render :: w -> IO ()
    }


instance Semigroup (System w) where
    (<>) :: System w -> System w -> System w
    (<>) sys1 sys2 =
        System
            { handleInput = handleInput sys1 >=> handleInput sys2,
              update = update sys2 . update sys1,
              render = \w -> render sys1 w >> render sys2 w
            }


instance Monoid (System w) where
    mempty :: System w
    mempty = System return id (\_ -> return ())


gameLoop :: System w -> IO Bool -> w -> IO ()
gameLoop sys@System{handleInput, update, render} shouldExitFunc world = do
    shouldExit <- shouldExitFunc
    unless shouldExit $ do
        world' <- handleInput world
        let world'' = update world'
        render world''
        gameLoop sys shouldExitFunc world''