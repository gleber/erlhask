{-# LANGUAGE TemplateHaskell #-}

-- | Main entry point to the application.
module ErlBifErlang (exportedMod) where

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Network.Transport.TCP

import Control.Monad.Trans.Class (lift)
import Control.Monad.State (liftIO)
import qualified Data.Map as M

import Control.Monad.State (get)

import ErlCore
import ErlBifsCommon


erlang_display, erlang_minus, erlang_plus, erlang_self :: [ErlTerm] -> ErlProcessState ErlTerm

erlang_display (arg:[]) = do
  liftIO $ print arg
  return arg
erlang_display _ = bif_badarg_num

erlang_minus (a:b:[]) =
  return $ case (a, b) of
    (ErlNum aa, ErlNum bb) -> ErlNum (aa - bb)
    (ErlNum aa, ErlFloat bb) -> ErlFloat (fromInteger aa - bb)
    (ErlFloat aa, ErlNum bb) -> ErlFloat (aa - fromInteger bb)
    (ErlFloat aa, ErlFloat bb) -> ErlFloat (aa - bb)
    _ -> bif_badarg_t
erlang_minus _ = bif_badarg_num

erlang_plus (a:b:[]) =
  return $ case (a, b) of
    (ErlNum aa, ErlNum bb) -> ErlNum (aa + bb)
    (ErlNum aa, ErlFloat bb) -> ErlFloat (fromInteger aa + bb)
    (ErlFloat aa, ErlNum bb) -> ErlFloat (aa + fromInteger bb)
    (ErlFloat aa, ErlFloat bb) -> ErlFloat (aa + bb)
    _ -> bif_badarg_t
erlang_plus _ = bif_badarg_num

erlang_self [] = do
  pid <- lift $ getSelfPid
  return $ ErlPid pid

exportedMod :: ErlModule
exportedMod =
  HModule "erlang" (M.fromList [(("display", 1), erlang_display),
                                (("-", 2), erlang_minus),
                                (("+", 2), erlang_plus),
                                (("self", 0), erlang_self)
                                -- (("spawn", 1), erlang_spawn) - implemented directly in the ErlEval
                                -- (("apply", 2), erlang_apply) - implemented directly in the ErlEval
                               ])
