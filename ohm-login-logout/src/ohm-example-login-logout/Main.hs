module Main where

import           Control.Monad (forever)
import qualified Control.Exception as Ex
import qualified Ohm
import           Prelude
import qualified ExampleLoginLogout.UI.Widgets.Root as UI

--------------------------------------------------------------------------------

main :: IO ()
main = Ohm.run Ex.bracket monitor UI.def UI.model UI.controller UI.viewRoot
  where
    monitor mdbg = forever $ do
      dbg <- mdbg
      putStrLn $ "MONITOR: " ++ show dbg
