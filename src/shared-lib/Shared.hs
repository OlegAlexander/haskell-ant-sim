module Shared where

import Control.Monad (unless, (>=>))
import Raylib.Types (Vector2 (..))
import Raylib.Util.Math (deg2Rad)


getNextPos :: Float -> Float -> Float -> Vector2 -> Vector2
getNextPos angle speed stepSize (Vector2 x y) =
    let rad = (-angle) * deg2Rad -- negate angle because of screen space coords
        x' = x + stepSize * speed * cos rad
        y' = y + stepSize * speed * sin rad
    in  Vector2 x' y'


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