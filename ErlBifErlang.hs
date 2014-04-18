-- | Main entry point to the application.
module ErlBifErlang (exportedMod) where

import Control.Monad.State (liftIO)
import qualified Data.Map as M

import ErlCore
import ErlBifsCommon

erlang_spawn :: [ErlTerm] -> ErlProcessState ErlTerm
erlang_spawn (arg:[]) = do
  undefined
erlang_spawn _ = bif_badarg_num

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

exportedMod :: ErlModule
exportedMod =
  HModule "erlang" (M.fromList [(("display", 1), erlang_display),
                                (("-", 2), erlang_minus),
                                (("+", 2), erlang_plus),
                                (("spawn", 1), erlang_spawn)
                               ])
