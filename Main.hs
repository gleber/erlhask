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

import Control.Exception (Exception(..), throw, throwIO, SomeException)

import Control.Monad.State (evalStateT)

import ErlCore
import qualified ErlBifs as Bifs

import ErlUtil
import ErlEval
import ErlModules

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
