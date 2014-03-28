module ErlCore where


import System.Random (getStdRandom, randomIO)

import Control.Monad.State
  (State, StateT, put, get, runState, evalState, evalStateT, runStateT,
   liftIO)
import Data.Map as M

import qualified Data.Map as M
import qualified Data.List as L

import Language.CoreErlang.Parser as P
import Language.CoreErlang.Syntax as S
import Language.CoreErlang.Pretty as PP

type FunName = String
type Arity = Integer
type Key = String

data ErlTerm = ErlList [ErlTerm] |
               ErlTuple [ErlTerm] |
               ErlAtom String |
               ErlNum Integer |
               ErlFloat Double
               -- ErlBitstring |
               -- ErlPid |
               -- ErlPort |
               -- ErlRef


instance Show ErlTerm where
  show (ErlAtom atom) = concat ["'", show atom, "'"]
  show (ErlNum num) = show num
  show (ErlFloat double) = show double
  show (ErlList list) = L.concat ["[", L.intercalate ", " $ L.map show list, "]"]
  show (ErlTuple tuple) = L.concat ["{", L.intercalate ", " $ L.map show tuple, "}"]

type VarTable = M.Map String ErlTerm
type ProcessDictionary = M.Map String ErlTerm
type ModTable = M.Map String ErlModule

type ErlFunHead = (FunName, Arity)
type ErlFun = ([ErlTerm] -> ErlProcessState ErlTerm)

data ErlModule = EModule S.Module |
                 HModule (M.Map ErlFunHead ErlFun)

data EvalCtx = ECtx VarTable
type ErlProcessState a = StateT (ModTable, ProcessDictionary) IO a
