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

instance Eq ErlTerm where
  (==) (ErlLambda a _ _ _) (ErlLambda b _ _ _) = a == b
  (==) (ErlAtom a) (ErlAtom b) = a == b

data ErlTerm = ErlList [ErlTerm] |
               ErlTuple [ErlTerm] |
               ErlAtom String |
               ErlNum Integer |
               ErlFloat Double |
               ErlFunName FunName ErlArity |
               ErlLambda FunName [Var] EvalCtx S.Exps
             deriving (Generic, Typeable)
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

data ErlModule = EModule S.Module |
                 HModule (M.Map ErlFunHead ErlFun)
               deriving (Generic, Typeable)

bootModule :: ErlModule
bootModule = HModule (M.empty)

data EvalCtx = ECtx VarTable
     deriving (Generic)

type ErlProcessState a = StateT (ErlModule, ModTable, ProcessDictionary) Process a

instance Binary ErlTerm
instance Binary EvalCtx

-- deriving instance Generic (S.List a)
-- deriving instance Generic S.Pats
-- deriving instance Generic S.Pat
-- deriving instance Generic S.Literal
-- deriving instance Generic S.Const
-- deriving instance Generic S.Function
-- deriving instance Generic S.Guard
-- deriving instance Generic S.TimeOut
-- deriving instance Generic S.Atom
-- deriving instance Generic S.Alt
-- deriving instance Generic S.Alias
-- deriving instance Generic S.FunDef
-- deriving instance Generic (S.Ann a)
-- deriving instance Generic (S.BitString a)
-- deriving instance Generic S.Exp
-- deriving instance Generic S.Exps
-- instance Binary (S.List S.Const)
-- instance Binary (S.List S.Exps)
-- instance Binary (S.List S.Pat)
-- instance Binary S.Const
-- instance Binary S.TimeOut
-- instance Binary S.Atom
-- instance Binary S.FunDef
-- instance Binary S.Alt
-- instance Binary S.Alias
-- instance Binary S.Pat
-- instance Binary S.Pats
-- instance Binary S.Literal
-- instance Binary (S.Ann S.Function)
-- instance Binary S.Function
-- instance Binary S.Guard
-- instance Binary (S.BitString S.Exps)
-- instance Binary (S.BitString S.Pat)
-- instance Binary (S.Ann [Ann Exp])
-- instance Binary (S.Ann Exp)
-- instance Binary (S.Ann Alt)
-- instance Binary S.Exps
-- instance Binary S.Exp
