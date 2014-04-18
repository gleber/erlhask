-- | Main entry point to the application.
module ErlBifs where

import ErlCore

import qualified Data.Map as M

import Control.Monad.State (liftIO)
import System.Random (randomIO)

import ErlBifErlang as Erlang
import ErlBifsCommon

random_uniform :: [ErlTerm] -> ErlProcessState ErlTerm
random_uniform [] = do
  value <- liftIO $ (randomIO :: IO Double)
  return $ ErlFloat value
random_uniform _ = bif_badarg_num

random :: ErlModule
random =
  HModule "random" (
    M.fromList [(("uniform", 0), random_uniform)])

all :: [(String, ErlModule)]
all =
  [("random", random),
   ("erlang", Erlang.exportedMod)]
