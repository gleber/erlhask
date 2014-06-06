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

import Control.Concurrent.MVar
import Data.Global

--
-- PROCESS REGISTRATION
--

erlang_register, erlang_whereis :: ErlStdFun
erlang_register [ErlAtom name, ErlPid pid] = do
  -- TODO: catch already registered exception and
  -- return badarg
  lift $ lift $ register name pid
  return $ ErlAtom "ok"
erlang_register _ = bif_badarg_num

erlang_whereis [ErlAtom name] = do
  n <- lift $ lift $ whereis name
  case n of
    Just pid ->
      return $ ErlPid pid
    Nothing ->
      return $ ErlAtom "undefined"
erlang_whereis _ = bif_badarg_num

--
-- ERRORS RAISING AND HANDLING
--

erlang_error, erlang_display :: ErlStdFun

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
  case exc of
    Nothing ->
      return $ ErlList []
    Just exc' ->
      return $ stacktraceToTerm (tail $ stack exc')
erlang_get_stacktrace _ = bif_badarg_num

--
-- DEBUG
--

erlang_display (arg:[]) = do
  liftIO $ print arg
  return $ ErlAtom "true"
erlang_display _ = bif_badarg_num

--
-- PROCESS MANAGEMENT
--

erlang_self, erlang_send, erlang_process_flag :: ErlStdFun

erlang_process_flag (ErlAtom what:val:[]) = do
  case what of
    "trap_exit" -> do
      liftIO $ putStrLn "erlang:process_flag(trap_exit, _) is not really implemented"
      return $ ErlAtom "true"
    _ ->
      bif_notimpl
erlang_process_flag (_:_:[]) = do
  bif_badarg_t
erlang_process_flag _ = do
  bif_badarg_num

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

--
-- MATHS
--

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

--
-- EXPORTS
--

exportedMod :: ErlModule
exportedMod =
  HModule "erlang" (M.fromList [(("display", 1), ErlStdFun erlang_display),
                                (("!", 2), ErlStdFun erlang_send),
                                (("self", 0), ErlStdFun erlang_self),
                                (("error", 1), ErlStdFun erlang_error),
                                (("throw", 1), ErlStdFun erlang_throw),
                                (("get_stacktrace", 0), ErlStdFun erlang_get_stacktrace),
                                (("exit", 1), ErlStdFun erlang_exit),

                                (("process_flag", 2), ErlStdFun erlang_process_flag),

                                (("register", 2), ErlStdFun erlang_register),
                                (("whereis", 1), ErlStdFun erlang_whereis),

                                (("-", 2), ErlPureFun erlang_minus),
                                (("+", 2), ErlPureFun erlang_plus),
                                (("float", 1), ErlPureFun erlang_float)
                                -- (("spawn", 1), erlang_spawn) - implemented directly in the ErlEval
                                -- (("apply", 2), erlang_apply) - implemented directly in the ErlEval
                               ])
