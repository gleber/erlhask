module ErlModules where

import Language.CoreErlang.Parser as P
import Language.CoreErlang.Pretty as PP
import Language.CoreErlang.Syntax as S

import qualified Data.Map as M
import qualified Data.List as L
import Data.Char as C

import Data.Either.Utils

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (
  put,
  get,
  modify)

import ErlCore
import ErlLangCore

ensureEModule :: String -> ErlProcessState ErlModule
ensureEModule moduleName = do
  (_, modTable, _) <- get
  case M.lookup moduleName modTable of
    Just emodule ->
      return emodule
    Nothing -> do
      r <- liftIO $ loadEModule modTable moduleName
      let (emodule, modTable') = forceEither $ r
      modify $ \(a, _, c) ->
        (a, modTable', c)
      return emodule

loadEModule :: ModTable -> String -> IO (Either String (ErlModule, ModTable))
loadEModule modTable moduleName = do
  res <- loadEModule0 moduleName
  case res of
    Left er -> return $ Left er
    Right m -> do
      let m' = EModule moduleName m
      let modTable' = M.insert moduleName m' modTable
      return $ Right (m', modTable')

loadEModule0 :: String -> IO (Either String S.Module)
loadEModule0 moduleName = do
  fileContent <- readFile ("samples/" ++ moduleName ++ ".core")
  case P.parseModule fileContent of
    Left er ->  do
      return $ Left $ show er
    Right m -> do
      putStrLn $ prettyPrint m
      return $ Right (unann m)
