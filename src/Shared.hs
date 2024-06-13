module Shared where

import Control.Monad (unless)


gameLoop :: (w -> IO w) -> (w -> w) -> (w -> IO ()) -> (w -> Bool) -> w -> IO ()
gameLoop handleInputFunc updateFunc renderFunc shouldExitFunc world = do
    world' <- handleInputFunc world
    unless (shouldExitFunc world') $ do
        let world'' = updateFunc world'
        renderFunc world''
        gameLoop handleInputFunc updateFunc renderFunc shouldExitFunc world''