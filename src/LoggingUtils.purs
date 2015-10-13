module LoggingUtils where

import Control.Monad.Eff
import Control.Monad.Eff.Console hiding (error)
import Control.Monad.Eff.Exception

import Prelude

logError :: forall e. Error -> Eff (console :: CONSOLE | e) Unit
logError err = log $ "ERROR: " ++ message err
