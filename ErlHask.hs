{-# LANGUAGE TemplateHaskell, DataKinds, DeriveGeneric, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Main entry point to the application.
module Main where

import Prelude hiding (catch)

import Debug.HTrace

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Network.Transport.TCP

import Data.Hashable
import Data.Binary (Binary)
import Data.Typeable
import GHC.Generics
import Data.Either.Utils

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Applicative ((<$>))
import Control.Exception (Exception(..), throw, throwIO, SomeException)

import Control.Concurrent
import Control.Concurrent.MVar

import qualified Data.Map as M
import qualified Data.List as L
import Data.Char as C

import Control.Monad.State (
  put,
  get,
  modify,
  evalStateT)

import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)

import Language.CoreErlang.Parser as P
import Language.CoreErlang.Syntax as S
import Language.CoreErlang.Pretty as PP

import ErlCore
import qualified ErlBifs as Bifs

import ErlUtil
import ErlEval
import ErlModules

newBifsModTable :: ModTable
newBifsModTable =
  let bifs = Bifs.all
  in M.fromList bifs

evaluator :: (ModName, FunName, [ErlTerm]) -> Process ()
evaluator (emod, fn, args) = do
  liftIO $ putStrLn "Evaluating"
  let runner = applyMFA emod fn args
  let eval = do
        result <- evalStateT runner (bootModule, newBifsModTable, newProcDict)
        liftIO $ putStrLn "Done"
        liftIO $ print result
        return ()
  catch eval (\(e :: SomeException) -> do
                 liftIO $ print (show e)
                 throw e)
remotable ['evaluator]


bootProc :: Process ()
bootProc = do
  liftIO $ putStrLn "Boot process starting..."
  liftIO $ putStrLn "Running"
  pid <- spawnLocal (evaluator ("boot", "start", []))
  mref <- monitor pid
  a <- expect :: Process ProcessMonitorNotification
  liftIO $ print $ show a
  return ()


-- | The main entry point.
main :: IO ()
main = do
  putStrLn "Booting..."
  tr <- createTransport "localhost" "8981" defaultTCPParameters
  case tr of
    Left e -> do
      putStrLn "Faield to create a transport"
      print e
    Right transport -> do
      putStrLn "Transport created."
      node <- newLocalNode transport initRemoteTable
      putStrLn "Node created."
      putStrLn "Running boot process..."
      runProcess node bootProc
      putStrLn "Boot process terminated"
