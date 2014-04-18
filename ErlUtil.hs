module ErlUtil where

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Network.Transport.TCP

import Control.Monad.Trans.Class (lift)

import qualified Data.List as L
import ErlCore

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

dieL :: [String] -> ErlProcessState x
dieL args = lift $ die (L.intercalate " " args)

showShortFunName :: String -> ErlArity -> String
showShortFunName fn arity =
  concat [fn, "/", show arity]

showFunName :: String -> String -> ErlArity -> String
showFunName emod fn arity =
  concat [emod, ":", fn, "/", show arity]

showFunCall :: String -> String -> [ErlTerm] -> String
showFunCall emod fn args =
  concat [emod, ":", fn, "(", L.intercalate "," (L.map show args), ")"]
