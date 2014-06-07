module Language.Erlang.Lang where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Char as C

import Language.CoreErlang.Syntax as S

import Language.Erlang.Core

unann :: Ann a -> a
unann (Constr a) = a
unann (Ann a _) = a

literalToTerm :: Literal -> ErlTerm
literalToTerm (LChar c) = ErlNum $ toInteger $ C.ord c
literalToTerm (LString str) = ErlList $ L.map (ErlNum . toInteger . C.ord) str
literalToTerm (LInt int) = ErlNum int
literalToTerm (LFloat double) = ErlFloat double
literalToTerm (LAtom (Atom atom_name)) = ErlAtom atom_name
literalToTerm LNil = ErlList []

data ErlSeq = ErlSeq [ErlTerm]
            deriving Show

unseq :: ErlSeq -> ErlTerm
unseq (ErlSeq [a]) = a
unseq (ErlSeq x) = error ("Expected exactly one term, instead got" ++ show x)

unseqM :: Functor f => f ErlSeq -> f ErlTerm
unseqM = fmap unseq
