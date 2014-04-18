{-# LANGUAGE DeriveGeneric, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances #-}


module ErlCore where

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


type ModName = String
type FunName = String
type ErlArity = Integer
type Key = String

-- instance Eq ErlTerm where
--   (==) (ErlLambda a _ _ _) (ErlLambda b _ _ _) = a == b
--   (==) (ErlAtom a) (ErlAtom b) = a == b

data ErlTerm = ErlList [ErlTerm] |
               ErlTuple [ErlTerm] |
               ErlAtom String |
               ErlNum Integer |
               ErlFloat Double |
               ErlFunName FunName ErlArity |
               ErlLambda FunName [Var] EvalCtx S.Exps
             deriving (Generic, Typeable, Eq)
-- ErlBitstring |
-- ErlPid |
-- ErlPort |
-- ErlRef


instance Show ErlTerm where
  show (ErlAtom atom) = concat ["'", atom, "'"]
  show (ErlNum num) = show num
  show (ErlFloat double) = show double
  show (ErlList list) = L.concat ["[", L.intercalate ", " $ L.map show list, "]"]
  show (ErlTuple tuple) = L.concat ["{", L.intercalate ", " $ L.map show tuple, "}"]
  show (ErlFunName fn arity) = fn ++ "/" ++ (show arity)
  show (ErlLambda name _ _ _) = "#Fun<" ++ name ++ ">"

instance Hashable S.Exps where
  hashWithSalt salt exprs = hashWithSalt salt (show exprs)

type VarTable = M.Map String ErlTerm
type ProcessDictionary = M.Map String ErlTerm
type ModTable = M.Map String ErlModule

type ErlMFA = (ModName, FunName, ErlArity)
type ErlFunHead = (FunName, ErlArity)
type ErlFun = ([ErlTerm] -> ErlProcessState ErlTerm)
type ErlFunTable = M.Map ErlFunHead ErlFun

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

type ErlProcessState a = StateT (ErlModule, ModTable, ProcessDictionary) Process a

instance Binary ErlTerm
instance Binary EvalCtx


newEvalCtx :: EvalCtx
newEvalCtx = ECtx M.empty

newProcDict :: ProcessDictionary
newProcDict = M.empty

newModTable :: ModTable
newModTable = M.empty

newProcState :: ErlModule -> (ErlModule, ModTable, ProcessDictionary)
newProcState emod = (emod, newModTable, newProcDict)
