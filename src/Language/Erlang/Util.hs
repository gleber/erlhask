{-# LANGUAGE DeriveGeneric, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances, Rank2Types, FlexibleContexts #-}

module Language.Erlang.Util where

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Network.Transport.TCP

import Control.Monad.Trans.Class (lift)

import qualified Data.List as L
import qualified Data.Map as M

import Language.CoreErlang.Syntax as S

import Language.Erlang.Core
import Language.Erlang.Lang

forceMaybeL :: Maybe a -> [String] -> a
forceMaybeL m e =
  forceMaybe m (L.intercalate " " e)

forceMaybe :: Maybe a -> String -> a
forceMaybe m e =
  case m of
    Just val ->
      val
    Nothing ->
      error e

maybeError :: String -> Maybe a -> Either String a
maybeError what = maybe (Left what) Right

maybeErrorL :: [String] -> Maybe a -> Either String a
maybeErrorL what = maybe (Left (L.intercalate " " what)) Right

orFail :: Maybe a -> String -> Either String a
orFail = flip maybeError

orFailL :: Maybe a -> [String] -> Either String a
orFailL = flip maybeErrorL

errorL :: [String] -> a
errorL args = error (L.intercalate " " args)

dieL :: [String] -> ErlProcess x
dieL args = lift $ fail (L.intercalate " " args)

showShortFunName :: String -> ErlArity -> String
showShortFunName fn arity =
  concat [fn, "/", show arity]

showFunName :: String -> String -> ErlArity -> String
showFunName emod fn arity =
  concat [emod, ":", fn, "/", show arity]

showFunCall :: String -> String -> [ErlTerm] -> String
showFunCall emod fn args =
  concat [emod, ":", fn, "(", L.intercalate "," (L.map show args), ")"]

setupFunctionContext :: EvalCtx -> ([Var], ErlSeq) -> EvalCtx
setupFunctionContext eCtx ([], _) = eCtx
setupFunctionContext (ECtx varTable) (vars, ErlSeq terms) =
  let pairs = L.zip vars terms
  in ECtx $ L.foldl (\vt (v,t) -> M.insert v t vt) varTable pairs

setupFunctionContext' :: EvalCtx -> [([Var], ErlSeq)] -> EvalCtx
setupFunctionContext' eCtx args =
  L.foldl (\oCtx arg -> setupFunctionContext oCtx arg) eCtx args
