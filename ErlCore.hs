{-# LANGUAGE DeriveGeneric, StandaloneDeriving, TemplateHaskell, DeriveDataTypeable #-}

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

instance Eq ErlTerm where
  (==) (ErlLambda a _ _) (ErlLambda b _ _) = a == b
  (==) (ErlAtom a) (ErlAtom b) = a == b

data ErlTerm = ErlList [ErlTerm] |
               ErlTuple [ErlTerm] |
               ErlAtom String |
               ErlNum Integer |
               ErlFloat Double |
               ErlFunName FunName ErlArity |
               ErlLambda FunName [Var] ErlFun
             deriving (Generic, Typeable)

instance Binary ErlTerm

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
  show (ErlLambda name _ _) = "#Fun<" ++ name ++ ">"

instance Hashable S.Exps where
  hashWithSalt salt exprs = hashWithSalt salt (show exprs)

type VarTable = M.Map String ErlTerm
type ProcessDictionary = M.Map String ErlTerm
type ModTable = M.Map String ErlModule

type ErlMFA = (ModName, FunName, ErlArity)
type ErlFunHead = (FunName, ErlArity)
type ErlFun = ([ErlTerm] -> ErlProcessState ErlTerm)

data ErlModule = EModule S.Module |
                 HModule (M.Map ErlFunHead ErlFun)
               deriving (Generic, Typeable)

bootModule :: ErlModule
bootModule = HModule (M.empty)

data EvalCtx = ECtx VarTable
type ErlProcessState a = StateT (ErlModule, ModTable, ProcessDictionary) Process a
