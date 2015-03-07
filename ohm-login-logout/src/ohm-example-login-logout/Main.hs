module Main where

import qualified Control.Exception as Ex
import qualified Ohm
import           Prelude
import qualified ExampleLoginLogout.UI.Widgets.Root as UI

--------------------------------------------------------------------------------

main :: IO ()
main = Ohm.run Ex.bracket monitor UI.modelDef UI.controller UI.viewRoot
  where
    monitor _ = return ()
--    monitor mdbg = forever $ do
--      dbg <- mdbg
--      putStrLn $ "MONITOR: " ++ show dbg
