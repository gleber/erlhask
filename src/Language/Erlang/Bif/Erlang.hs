{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts #-}

-- | Main entry point to the application.
module Language.Erlang.Bif.Erlang (exportedMod) where

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Network.Transport.TCP

import Control.Monad.Trans.Class (lift)
import Control.Monad.State (liftIO)
import qualified Data.Map as M

import Control.Monad.State (get, gets)
import Control.Monad.Reader (ask)
import Control.Monad.Error (throwError)

import Language.Erlang.Core
import Language.Erlang.BifsCommon


erlang_error, erlang_display, erlang_self, erlang_send :: ErlStdFun

erlang_error (arg:[]) = do
  stack <- ask
  throwError $ ErlException { exc_type = ExcError, reason = arg, stack = stack }
erlang_error _ = bif_badarg_num

erlang_exit (arg:[]) = do
  stack <- ask
  throwError $ ErlException { exc_type = ExcExit, reason = arg, stack = stack }
erlang_exit _ = bif_badarg_num

erlang_throw (arg:[]) = do
  stack <- ask
  throwError $ ErlException { exc_type = ExcThrow, reason = arg, stack = stack }
erlang_throw _ = bif_badarg_num

erlang_get_stacktrace [] = do
  exc <- gets last_exc
  return $ stacktraceToTerm (tail $ stack exc)
erlang_get_stacktrace _ = bif_badarg_num

erlang_display (arg:[]) = do
  liftIO $ print arg
  return $ ErlAtom "true"
erlang_display _ = bif_badarg_num

erlang_self [] = do
  pid <- lift $ lift $ getSelfPid
  return $ ErlPid pid

erlang_send (pid:msg:[]) = do
  case (pid, msg) of
    (ErlPid p, _) -> do
      lift $ lift $ send p msg
      return msg
    _ ->
      bif_badarg_t
erlang_send _ = bif_badarg_num

erlang_minus, erlang_plus, erlang_float :: ErlPureFun

erlang_minus (a:b:[]) =
  case (a, b) of
    (ErlNum aa, ErlNum bb) -> return $ ErlNum (aa - bb)
    (ErlNum aa, ErlFloat bb) -> return $ ErlFloat (fromInteger aa - bb)
    (ErlFloat aa, ErlNum bb) -> return $ ErlFloat (aa - fromInteger bb)
    (ErlFloat aa, ErlFloat bb) -> return $ ErlFloat (aa - bb)
    _ -> bif_badarg_t
erlang_minus _ = bif_badarg_num

erlang_plus (a:b:[]) =
  case (a, b) of
    (ErlNum aa, ErlNum bb) -> return $ ErlNum (aa + bb)
    (ErlNum aa, ErlFloat bb) -> return $ ErlFloat (fromInteger aa + bb)
    (ErlFloat aa, ErlNum bb) -> return $ ErlFloat (aa + fromInteger bb)
    (ErlFloat aa, ErlFloat bb) -> return $ ErlFloat (aa + bb)
    _ -> bif_badarg_t
erlang_plus _ = bif_badarg_num

erlang_float (a:[]) =
  case a of
    ErlFloat a -> return $ ErlFloat a
    ErlNum a -> return $ ErlFloat $ fromInteger a
    _ -> bif_badarg_t
erlang_float _ = bif_badarg_num

exportedMod :: ErlModule
exportedMod =
  HModule "erlang" (M.fromList [(("display", 1), ErlStdFun erlang_display),
                                (("!", 2), ErlStdFun erlang_send),
                                (("self", 0), ErlStdFun erlang_self),
                                (("error", 1), ErlStdFun erlang_error),
                                (("throw", 1), ErlStdFun erlang_throw),
                                (("get_stacktrace", 0), ErlStdFun erlang_get_stacktrace),
                                (("exit", 1), ErlStdFun erlang_exit),

                                (("-", 2), ErlPureFun erlang_minus),
                                (("+", 2), ErlPureFun erlang_plus),
                                (("float", 1), ErlPureFun erlang_float)
                                -- (("spawn", 1), erlang_spawn) - implemented directly in the ErlEval
                                -- (("apply", 2), erlang_apply) - implemented directly in the ErlEval
                               ])
