module Shared where

import Control.Monad (unless, (>=>))


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
    mempty = System{handleInput = return, update = id, render = \_ -> return ()}


gameLoopSys :: System w -> IO Bool -> w -> IO ()
gameLoopSys sys@System{handleInput, update, render} shouldExitFunc world = do
    shouldExit <- shouldExitFunc
    unless shouldExit $ do
        world' <- handleInput world
        let world'' = update world'
        render world''
        gameLoopSys sys shouldExitFunc world''


gameLoop :: (w -> IO w) -> (w -> w) -> (w -> IO ()) -> IO Bool -> w -> IO ()
gameLoop handleInputFunc updateFunc renderFunc shouldExitFunc world = do
    shouldExit <- shouldExitFunc
    unless shouldExit $ do
        world' <- handleInputFunc world
        let world'' = updateFunc world'
        renderFunc world''
        gameLoop handleInputFunc updateFunc renderFunc shouldExitFunc world''