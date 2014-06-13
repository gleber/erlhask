{-# LANGUAGE TemplateHaskell, RankNTypes, FlexibleContexts #-}

-- | Main entry point to the application.
module Language.Erlang.Bif.Erlang (exportedMod,
                                   erlang_link) where

import Debug.HTrace

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Network.Transport.TCP

import Control.Monad.Trans.Class (lift)
import Control.Monad.State (liftIO)
import qualified Data.List as L
import qualified Data.Char as C
import qualified Data.Map as M

import Control.Monad.State (get, gets)
import Control.Monad.Reader (ask)
import Control.Monad.Error (throwError)

import Language.Erlang.Core
import Language.Erlang.BifsCommon

import Control.Concurrent.MVar
import Data.Global
import Data.Binary
import qualified Data.ByteString.Lazy as BS

import Control.Distributed.Process.Internal.Types (ProcessSignal(..), Identifier(..))
import Control.Distributed.Process.Internal.Primitives (sendCtrlMsg)


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

erlang_self, erlang_send, erlang_process_flag, erlang_link :: ErlStdFun

erlang_link [ErlPid them] = do
  us <- lift $ lift $ getSelfPid
  let theirNode = processNodeId them
  lift $ lift $ sendCtrlMsg Nothing (Link (ProcessIdentifier them))
  lift $ lift $ sendCtrlMsg (Just theirNode) (Link (ProcessIdentifier us))
  return $ ErlAtom "true"

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

erlang_concat [ErlList a, ErlList b] =
  return $ ErlList (a ++ b)
erlang_concat [_, _] = bif_badarg_t
erlang_concat _ = bif_badarg_num

erlang_bin_bool_op :: (ErlTerm -> ErlTerm -> Bool) -> [ErlTerm] -> ErlGeneric ErlTerm
erlang_bin_bool_op op [a, b] =
  return $ case op a b of
    True -> atom_true
    False -> atom_false
erlang_bin_bool_op _ _ = bif_badarg_num

erlang_lt, erlang_gt, erlang_eq, erlang_neq, erlang_lte, erlang_gte, erlang_eq_ex, erlang_neq_ex :: ErlPureFun
erlang_lt = erlang_bin_bool_op (<)
erlang_gt = erlang_bin_bool_op (>)
erlang_eq = erlang_bin_bool_op (==)
erlang_neq = erlang_bin_bool_op (/=)
erlang_lte = erlang_bin_bool_op (<=)
erlang_gte = erlang_bin_bool_op (>=)
erlang_eq_ex = erlang_eq --FIXME: that's a hack!
erlang_neq_ex = erlang_neq --FIXME: that's a hack!


erlang_logic_bool_op :: (Bool -> Bool -> Bool) -> [ErlTerm] -> ErlGeneric ErlTerm
erlang_logic_bool_op op [a@(ErlAtom _), b@(ErlAtom _)] | erlIsBool a && erlIsBool b = do
  aa <- erlToBool a
  bb <- erlToBool b
  return $ case op aa bb of
    True -> atom_true
    False -> atom_false
erlang_logic_bool_op op [_, _] = bif_badarg_t
erlang_logic_bool_op _ _ = bif_badarg_num

xor :: Bool -> Bool -> Bool
xor True x = not x
xor False x =x

erlang_and, erlang_or, erlang_xor, erlang_not :: ErlPureFun
erlang_and = erlang_logic_bool_op (&&)
erlang_or = erlang_logic_bool_op (||)
erlang_xor = erlang_logic_bool_op xor

erlang_not [a@(ErlAtom _)] | erlIsBool a = do
  aa <- erlToBool a
  return $ case aa of
    True -> atom_false
    False -> atom_true
erlang_not [_] = bif_badarg_t
erlang_not _ = bif_badarg_num

--
-- BINARIES
--

erlang_binary_to_list, erlang_list_to_binary, erlang_atom_to_list, erlang_list_to_atom :: ErlPureFun
erlang_binary_to_list [ErlBinary bs] =
  return $ ErlList $ L.map (ErlNum . toInteger) $ BS.unpack bs
erlang_binary_to_list [_] = bif_badarg_t
erlang_binary_to_list _ = bif_badarg_num

erlang_list_to_binary [ErlList l] =
  return $ ErlBinary $ BS.pack $ L.map (fromInteger . erlToInt) l
erlang_list_to_binary [_] = bif_badarg_t
erlang_list_to_binary _ = bif_badarg_num

erlang_atom_to_list [ErlAtom a] =
  return $ ErlList $ L.map (ErlNum . toInteger . C.ord) a
erlang_atom_to_list [_] = bif_badarg_t
erlang_atom_to_list _ = bif_badarg_num

erlang_list_to_atom [ErlList l] =
  return $ ErlAtom $ L.map (C.chr . fromInteger . erlToInt) l
erlang_list_to_atom [_] = bif_badarg_t
erlang_list_to_atom _ = bif_badarg_num

erlang_list_to_tuple [ErlList l] =
  return $ ErlTuple l
erlang_list_to_tuple [_] = bif_badarg_t
erlang_list_to_tuple _ = bif_badarg_num

erlang_tuple_to_list [ErlTuple l] =
  return $ ErlList l
erlang_tuple_to_list [_] = bif_badarg_t
erlang_tuple_to_list _ = bif_badarg_num

--
-- TYPES
--

erlang_is_atom [ErlAtom _] = return $ atom_true
erlang_is_atom [_] = return $ atom_false
erlang_is_atom _ = bif_badarg_num

erlang_is_list [ErlList _] = return $ atom_true
erlang_is_list [_] = return $ atom_false
erlang_is_list _ = bif_badarg_num

erlang_is_binary [ErlBinary _] = return $ atom_true
erlang_is_binary [_] = return $ atom_false
erlang_is_binary _ = bif_badarg_num

erlang_is_float [ErlFloat _] = return $ atom_true
erlang_is_float [_] = return $ atom_false
erlang_is_float _ = bif_badarg_num

erlang_is_integer [ErlNum _] = return $ atom_true
erlang_is_integer [_] = return $ atom_false
erlang_is_integer _ = bif_badarg_num

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

                                -- (("spawn", 1), erlang_spawn) - implemented directly in the ErlEval
                                -- (("apply", 2), erlang_apply) - implemented directly in the ErlEval
                                (("process_flag", 2), ErlStdFun erlang_process_flag),

                                (("register", 2), ErlStdFun erlang_register),
                                (("whereis", 1), ErlStdFun erlang_whereis),

                                (("link", 1), ErlStdFun erlang_link),

                                (("==", 2), ErlPureFun erlang_eq),
                                (("/=", 2), ErlPureFun erlang_neq),
                                (("=<", 2), ErlPureFun erlang_lte),
                                (("<", 2), ErlPureFun erlang_lt),
                                ((">=", 2), ErlPureFun erlang_gte),
                                ((">", 2), ErlPureFun erlang_gt),
                                (("=:=", 2), ErlPureFun erlang_eq_ex),
                                (("=/=", 2), ErlPureFun erlang_neq_ex),

                                (("not", 1), ErlPureFun erlang_not),
                                (("and", 2), ErlPureFun erlang_and),
                                (("or", 2), ErlPureFun erlang_or),
                                (("xor", 2), ErlPureFun erlang_xor),

                                (("-", 2), ErlPureFun erlang_minus),
                                (("+", 2), ErlPureFun erlang_plus),
                                (("float", 1), ErlPureFun erlang_float),
                                (("++", 2), ErlPureFun erlang_concat),

                                (("list_to_binary", 1), ErlPureFun erlang_list_to_binary),
                                (("binary_to_list", 1), ErlPureFun erlang_binary_to_list),

                                (("atom_to_list", 1), ErlPureFun erlang_atom_to_list),
                                (("list_to_atom", 1), ErlPureFun erlang_list_to_atom),

                                (("list_to_tuple", 1), ErlPureFun erlang_list_to_tuple),
                                (("tuple_to_list", 1), ErlPureFun erlang_tuple_to_list),

                                (("is_float", 1), ErlPureFun erlang_is_float),
                                (("is_integer", 1), ErlPureFun erlang_is_integer),
                                (("is_atom", 1), ErlPureFun erlang_is_atom),
                                (("is_list", 1), ErlPureFun erlang_is_list),
                                (("is_binary", 1), ErlPureFun erlang_is_binary)
                               ])
