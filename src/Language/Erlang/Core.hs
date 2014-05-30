{-# LANGUAGE DeriveGeneric, StandaloneDeriving, DeriveDataTypeable,
FlexibleInstances, Rank2Types, FlexibleContexts, RankNTypes,
DeriveGeneric, StandaloneDeriving, DeriveDataTypeable,
FlexibleInstances, RankNTypes, FlexibleContexts, ImpredicativeTypes #-}

module Language.Erlang.Core where

import Data.Unique
import Data.Binary
import Data.Typeable
import GHC.Generics

import Control.Distributed.Process

import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.RWS.Strict (RWST, runRWST)
import Control.Monad.Error (ErrorT, Error, runErrorT, throwError, strMsg)
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Char as C

import Language.CoreErlang.Syntax as S

import Control.Concurrent
import System.IO.Unsafe

threadId :: ThreadId
threadId = unsafePerformIO $ myThreadId

type ModName = String
type FunName = String
type ErlArity = Integer
type Key = String

data ErlTerm = ErlList [ErlTerm] |
               ErlTuple [ErlTerm] |
               ErlAtom String |
               ErlNum Integer |
               ErlFloat Double |
               ErlFunName FunName ErlArity |
               ErlLambda FunName [Var] EvalCtx S.Exps |
               ErlPid ProcessId |
               ErlRef Unique
             deriving (Generic, Typeable, Eq)
-- ErlBitstring |

isTimeout :: ErlTerm -> Bool
isTimeout (ErlNum _) = True
isTimeout (ErlAtom "infinity") = True
isTimeout _ = False

receive :: ErlTerm -> [Match b] -> Process (Maybe b)
receive (ErlNum time) matches = receiveTimeout (fromInteger time) matches
receive (ErlAtom "infinity") matches = do
  res <- receiveWait matches
  return $ Just res

instance Binary Unique where
  put = undefined
  get = undefined

instance Binary ErlTerm

erlIsInt :: ErlTerm -> Bool
erlIsInt (ErlNum _) = True
erlIsInt _ = False

erlToInt :: ErlTerm -> Integer
erlToInt (ErlNum a) = a
erlToInt _ = error "Not an integer"

instance Show ErlTerm where
  show (ErlAtom atom) = concat ["'", atom, "'"]
  show (ErlNum num) = show num
  show (ErlFloat double) = show double
  show (ErlList list) =
    case list /= [] && L.all erlIsInt list && L.all (C.isPrint . toEnum . fromInteger . erlToInt) list of
      True ->
        L.map (C.chr . fromInteger . erlToInt) list
      False ->
        L.concat ["[", L.intercalate ", " $ L.map show list, "]"]
  show (ErlTuple tuple) = L.concat ["{", L.intercalate ", " $ L.map show tuple, "}"]
  show (ErlFunName fn arity) = fn ++ "/" ++ (show arity)
  show (ErlLambda name _ _ _) = "#Fun<" ++ name ++ ">"
  show (ErlPid pid) = concat ["<", show pid, ">"]
  show (ErlRef uniq) = concat ["#Ref<", show (hashUnique uniq), ">"]

type VarTable = M.Map String ErlTerm
type ProcDict = M.Map String ErlTerm
type ModTable = M.Map String ErlModule

type ErlMFA = (ModName, FunName, ErlArity)
type ErlFunHead = (FunName, ErlArity)

type ErlFunTable = M.Map ErlFunHead ErlFun

-- TODO: unify both types to store lambdas with Maybe S.Exprs
data ErlModule = EModule ModName S.Module |
                 HModule ModName ErlFunTable
               deriving (Generic, Typeable)

modName :: ErlModule -> ModName
modName (EModule mn _) = mn
modName (HModule mn _) = mn

instance Show ErlModule where
  show (EModule modname _exps) = concat ["EModule<", modname, ">"]
  show (HModule modname _funs) = concat ["HModule<", modname, ">"]

bootModule :: ErlModule
bootModule = HModule "boot" (M.empty)

data EvalCtx = ECtx VarTable
     deriving (Generic, Eq)
instance Binary EvalCtx

data FilePos = FilePos String Integer

data StackFrame = Frame { mfa :: ErlMFA,
                          args :: Maybe [ErlTerm],
                          pos :: FilePos }

stackToTerm :: StackFrame -> ErlTerm
stackToTerm Frame { mfa = (mod, fun, arity) } =
  ErlTuple $ [ErlAtom mod,
              ErlAtom fun,
              ErlNum arity,
              ErlList []]

stacktraceToTerm :: [StackFrame] -> ErlTerm
stacktraceToTerm l =
  ErlList $ L.map stackToTerm l

data ErlExceptionType = ExcUnknown |
                        ExcError |
                        ExcThrow |
                        ExcExit

excTypeToTerm :: ErlExceptionType -> ErlTerm
excTypeToTerm t =
  case t of
    ExcUnknown -> ErlAtom "unknown"
    ExcError -> ErlAtom "error"
    ExcThrow -> ErlAtom "throw"
    ExcExit -> ErlAtom "exit"

excToTerm :: ErlException -> ErlTerm
excToTerm ErlException { exc_type = t,
                         reason = r,
                         stack = s } =
  let t' = excTypeToTerm t
  in ErlTuple [t', r, stacktraceToTerm s]

data ErlException = ErlException { exc_type :: ErlExceptionType,
                                   reason :: ErlTerm,
                                   stack :: [StackFrame] }
instance Error ErlException where
  strMsg str = ErlException { exc_type = ExcUnknown,
                              reason = ErlAtom str,
                              stack = [] }



data ErlPState = ErlPState { curr_mod :: ErlModule, -- move to reader
                             last_exc :: ErlException,
                             mod_table :: ModTable,
                             proc_dict :: ProcDict }

type ErlProcessState m = RWST [StackFrame] [String] ErlPState m
type ErlProcessEvaluator m = ErrorT ErlException (ErlProcessState m)
type ErlProcess = ErlProcessEvaluator Process

runErlProcess :: ErlProcess ErlTerm -> ErlModule -> ModTable -> ProcDict -> Process (Either ErlException ErlTerm)
runErlProcess p cm mt pd = do
  (res, _, _) <- runRWST (runErrorT p) [] (ErlPState { curr_mod = cm,
                                                       mod_table = mt,
                                                       proc_dict = pd })
  return res

type ErlPure = ErlProcessEvaluator Identity

runErlPure :: ModTable -> ErlPure ErlTerm -> Either ErlException ErlTerm
runErlPure mt p =
  let (res, _, _) = runIdentity $ runRWST (runErrorT p) [] (ErlPState {mod_table = mt})
  in res

type ErlGeneric a = Monad m => ErlProcessEvaluator m a
type ErlStdFun = ([ErlTerm] -> ErlProcess ErlTerm)
type ErlPureFun = ([ErlTerm] -> ErlGeneric ErlTerm)

data ErlFun = ErlStdFun ErlStdFun |
              ErlPureFun ErlPureFun

newEvalCtx :: EvalCtx
newEvalCtx = ECtx M.empty

newProcDict :: ProcDict
newProcDict = M.empty
