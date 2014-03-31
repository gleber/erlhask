-- | Main entry point to the application.
module ErlBifs where

import ErlCore

import Control.Monad.State (liftIO)
import qualified Data.Map as M

import System.Random (randomIO)

bif_badarg :: String -> a
bif_badarg str = error ("badarg<" ++ str ++ ">")

bif_badarg_num :: a
bif_badarg_num = bif_badarg "wrong-number"

bif_badarg_t :: a
bif_badarg_t = bif_badarg "wrong-types"

erlang_display :: [ErlTerm] -> ErlProcessState ErlTerm
erlang_display (arg:[]) = do
  liftIO $ print arg
  return arg
erlang_display _ = bif_badarg_num

erlang_minus :: [ErlTerm] -> ErlProcessState ErlTerm
erlang_minus (a:b:[]) =
  return $ case (a, b) of
    (ErlNum aa, ErlNum bb) -> ErlNum (aa - bb)
    (ErlNum aa, ErlFloat bb) -> ErlFloat (fromInteger aa - bb)
    (ErlFloat aa, ErlNum bb) -> ErlFloat (aa - fromInteger bb)
    (ErlFloat aa, ErlFloat bb) -> ErlFloat (aa - bb)
    _ -> bif_badarg_t
erlang_minus _ = bif_badarg_num

erlang_plus :: [ErlTerm] -> ErlProcessState ErlTerm
erlang_plus (a:b:[]) =
  return $ case (a, b) of
    (ErlNum aa, ErlNum bb) -> ErlNum (aa + bb)
    (ErlNum aa, ErlFloat bb) -> ErlFloat (fromInteger aa + bb)
    (ErlFloat aa, ErlNum bb) -> ErlFloat (aa + fromInteger bb)
    (ErlFloat aa, ErlFloat bb) -> ErlFloat (aa + bb)
    _ -> bif_badarg_t
erlang_plus _ = bif_badarg_num

erlang :: ErlModule
erlang =
  HModule (M.fromList [(("display", 1), erlang_display),
                       (("-", 2), erlang_minus),
                       (("+", 2), erlang_plus)
                      ])

random_uniform :: [ErlTerm] -> ErlProcessState ErlTerm
random_uniform [] = do
  value <- liftIO $ (randomIO :: IO Double)
  return $ ErlFloat value
random_uniform _ = bif_badarg_num

random :: ErlModule
random =
  HModule (
    M.fromList [(("uniform", 0), random_uniform)])

all :: [(String, ErlModule)]
all =
  [("random", random),
   ("erlang", erlang)]
