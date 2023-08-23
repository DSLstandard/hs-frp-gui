module Glew where

import Relude hiding (init)

foreign import ccall "GL/glew.h" glewInit :: IO ()

initialize :: MonadIO m => m ()
initialize = liftIO glewInit
