module Main where

import Prelude (Unit)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Hylogen


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!!!"
