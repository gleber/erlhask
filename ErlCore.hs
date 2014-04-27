{-# LANGUAGE DeriveGeneric, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances #-}

module ErlCore where

import Data.Unique
import Data.Binary
import Data.Typeable
import GHC.Generics
import Data.Hashable

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP

import Control.Monad.State (StateT)
import qualified Data.Map as M
import qualified Data.List as L

import Language.CoreErlang.Syntax as S

instance Hashable S.Exps where
  hashWithSalt salt exprs = hashWithSalt salt (show exprs)

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

instance Binary Unique where
  put = undefined
  get = undefined

instance Binary ErlTerm

instance Show ErlTerm where
  show (ErlAtom atom) = concat ["'", atom, "'"]
  show (ErlNum num) = show num
  show (ErlFloat double) = show double
  show (ErlList list) = L.concat ["[", L.intercalate ", " $ L.map show list, "]"]
  show (ErlTuple tuple) = L.concat ["{", L.intercalate ", " $ L.map show tuple, "}"]
  show (ErlFunName fn arity) = fn ++ "/" ++ (show arity)
  show (ErlLambda name _ _ _) = "#Fun<" ++ name ++ ">"
  show (ErlPid pid) = concat ["<", show pid, ">"]

type VarTable = M.Map String ErlTerm
type ProcessDictionary = M.Map String ErlTerm
type ModTable = M.Map String ErlModule

type ErlMFA = (ModName, FunName, ErlArity)
type ErlFunHead = (FunName, ErlArity)
type ErlStdFun = ([ErlTerm] -> ErlProcessState ErlTerm)
type ErlPureFun = ([ErlTerm] -> ErlTerm)
data ErlFun = ErlStdFun ErlStdFun | ErlPureFun ErlPureFun
type ErlFunTable = M.Map ErlFunHead ErlFun

-- TODO: unify both types to store lambdas with Maybe S.Exprs
data ErlModule = EModule ModName S.Module |
                 HModule ModName ErlFunTable
               deriving (Generic, Typeable)

instance Show ErlModule where
  show (EModule modname exps) = concat ["EModule<", modname, ">"]
  show (HModule modname _funs) = concat ["HModule<", modname, ">"]

bootModule :: ErlModule
bootModule = HModule "boot" (M.empty)

data EvalCtx = ECtx VarTable
     deriving (Generic, Eq)
instance Binary EvalCtx

type ErlProcessState a = StateT (ErlModule, ModTable, ProcessDictionary) Process a

newEvalCtx :: EvalCtx
newEvalCtx = ECtx M.empty

newProcDict :: ProcessDictionary
newProcDict = M.empty
