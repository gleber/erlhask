{-# LANGUAGE DeriveGeneric, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances, Rank2Types, FlexibleContexts #-}

module Language.Erlang.Modules where

import Language.CoreErlang.Parser as P
import Language.CoreErlang.Pretty as PP
import Language.CoreErlang.Syntax as S

import qualified Data.Map as M
import qualified Data.List as L
import Data.Char as C
import Data.Hashable

import Data.Either.Utils

import Control.Monad.RWS (gets, ask)
import Control.Monad.Error (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (
  put,
  get,
  modify)

import Control.Concurrent.MVar (readMVar, modifyMVarMasked)

import Language.Erlang.Core
import Language.Erlang.Lang

merge :: ErlModule -> ErlModule -> ErlModule
merge a b =
  b { funs = M.union (funs a) (funs b) }

getModTable :: ErlProcess ModTable
getModTable = do
  Left mvar <- gets mod_table
  liftIO $ readMVar mvar

ensureModule :: ModName -> ErlProcess ErlModule
ensureModule moduleName = do
  Left m <- gets mod_table
  emodule <- liftIO $ modifyMVarMasked m $ \mt ->
    case M.lookup moduleName mt of
      Just emodule ->
        return (mt, emodule)
      Nothing -> do
        Right (emodule, modTable') <- liftIO $ addCoreModule mt moduleName
        return (modTable', emodule)
  return emodule

safeGetModule :: ModName -> ErlPure ErlModule
safeGetModule moduleName = do
  Right modTable <- gets mod_table
  case M.lookup moduleName modTable of
    Just emodule ->
      return emodule
    Nothing -> do
      s <- getStackTrace
      throwError (ErlException { exc_type = ExcUnknown,
                                 reason = ErlAtom "module_not_found",
                                 stacktrace = s})

loadCoreModule :: String -> IO (Either String ErlModule)
loadCoreModule moduleName = do
  Right m <- loadCoreModule0 moduleName
  return $ Right ErlModule { mod_name = moduleName,
                             source = Just m,
                             funs = exportedFuns m }

addCoreModule :: ModTable -> String -> IO (Either String (ErlModule, ModTable))
addCoreModule modTable moduleName = do
  Right m <- loadCoreModule moduleName
  let modTable' = M.insert moduleName m modTable
  return $ Right (m, modTable')

loadCoreModule0 :: String -> IO (Either String S.Module)
loadCoreModule0 moduleName = do
  --TODO: replace with full path and leave path search to code server
  fileContent <- readFile ("samples/" ++ moduleName ++ ".core")
  case P.parseModule fileContent of
    Left er ->  do
      return $ Left $ show er
    Right m -> do
      -- putStrLn $ prettyPrint m
      return $ Right (unann m)

findExportedFunction :: String -> ErlArity -> [FunDef] -> Maybe FunDef
findExportedFunction name arity funs =
  let
    test = \(FunDef nm _) ->
      let (Function ((Atom n), a)) = unann nm
      in (n == name) && (a == arity)
  in
   L.find test funs

findFn :: String -> ErlArity -> [FunDef] -> Maybe ErlTerm
findFn name arity funs = do
  (FunDef _ aexp) <- findExportedFunction name arity funs
  let (Lambda vars exprs) = unann aexp
  return $ ErlLambda (show $ hash exprs) vars newEvalCtx exprs

findModFn :: S.Module -> String -> ErlArity -> Maybe ErlTerm
findModFn m name arity =
  let Module _modName _exports _attributes funs = m
  in
   findFn name arity funs

exportedFuns :: S.Module -> ErlFunTable
exportedFuns (Module name exports _ defs) =
  M.fromList $ (flip L.map) exports (\(Function (Atom name, arity)) ->
                                      let Just fd = findExportedFunction name arity defs
                                          FunDef _ aexp = fd
                                          Lambda argNames exps = unann aexp
                                      in
                                       ((name, arity), ErlCoreFun argNames exps))
