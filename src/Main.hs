{-# LANGUAGE TemplateHaskell, DataKinds, DeriveGeneric, StandaloneDeriving,
             DeriveDataTypeable, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point to the application.
module Main where

import Prelude hiding (catch)

import Debug.HTrace

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Network.Transport.TCP

import Control.Exception (Exception(..), throw, throwIO, SomeException)

import Control.Concurrent.MVar
import Control.Monad.State (evalStateT)

import Language.Erlang.Core
import qualified Language.Erlang.Bifs as Bifs

import Language.Erlang.Util
import Language.Erlang.Eval
import Language.Erlang.Modules



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

bootProc :: Process ()
bootProc = do
  mt <- liftIO $ newBaseModTable
  mmt <- liftIO $ newMVar mt
  liftIO $ putStrLn "Boot process starting..."
  liftIO $ putStrLn "Running"
  pid <- spawnLocal (localEvaluator bootModule mmt ("init", "boot", [ErlList [ErlBinary "-root"]]))
  mref <- monitor pid
  a <- expect :: Process ProcessMonitorNotification
  liftIO $ putStrLn $ "init process exit with: " ++ show a
  return ()
