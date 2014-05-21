{-# LANGUAGE DeriveGeneric, StandaloneDeriving, DeriveDataTypeable,
             FlexibleInstances, Rank2Types, FlexibleContexts #-}

module ErlCore where

import Data.Unique
import Data.Binary
import Data.Typeable
import GHC.Generics

import Control.Distributed.Process

import Control.Monad.State (StateT)
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
    case L.all erlIsInt list && L.all (C.isPrint . toEnum . fromInteger . erlToInt) list of
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
type ProcessDictionary = M.Map String ErlTerm
type ModTable = M.Map String ErlModule

type ErlMFA = (ModName, FunName, ErlArity)
type ErlFunHead = (FunName, ErlArity)

type ErlFunTable = M.Map ErlFunHead ErlFun

-- TODO: unify both types to store lambdas with Maybe S.Exprs
data ErlModule = EModule ModName S.Module |
                 HModule ModName ErlFunTable
               deriving (Generic, Typeable)

instance Show ErlModule where
  show (EModule modname _exps) = concat ["EModule<", modname, ">"]
  show (HModule modname _funs) = concat ["HModule<", modname, ">"]

bootModule :: ErlModule
bootModule = HModule "boot" (M.empty)

data EvalCtx = ECtx VarTable
     deriving (Generic, Eq)
instance Binary EvalCtx

type BaseErlProcessState m a = Monad m => StateT (ErlModule, ModTable, ProcessDictionary) m a

type ErlProcessState a = BaseErlProcessState Process a
type ErlPureState a = BaseErlProcessState Maybe a

type ErlStdFun = ([ErlTerm] -> ErlProcessState ErlTerm)
type ErlPureFun m = Monad m => ([ErlTerm] -> BaseErlProcessState m ErlTerm)

data ErlFun = ErlStdFun ErlStdFun |
              ErlPureFun (ErlPureFun Maybe)

newEvalCtx :: EvalCtx
newEvalCtx = ECtx M.empty

newProcDict :: ProcessDictionary
newProcDict = M.empty
