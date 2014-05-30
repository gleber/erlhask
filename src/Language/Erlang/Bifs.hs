-- | Main entry point to the application.
module Language.Erlang.Bifs where

import Language.Erlang.Core

import qualified Data.Map as M

import Control.Monad.State (liftIO)
import System.Random (randomIO)

import Language.Erlang.Bif.Erlang as Erlang
import Language.Erlang.BifsCommon

random_uniform :: ErlStdFun
random_uniform [] = do
  value <- liftIO $ (randomIO :: IO Double)
  return $ ErlFloat value
random_uniform _ = bif_badarg_num

random :: ErlModule
random =
  HModule "random" (
    M.fromList [(("uniform", 0), ErlStdFun random_uniform)])

modules :: [(String, ErlModule)]
modules =
  [("random", random),
   ("erlang", Erlang.exportedMod)]

newBifsModTable :: ModTable
newBifsModTable =
  M.fromList modules
