{-# LANGUAGE QuasiQuotes, ScopedTypeVariables #-}

import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Text.RawString.QQ

import qualified Data.Map as M

import Control.Concurrent.MVar
import Language.Erlang.Eval
import Language.Erlang.Core
import qualified Language.CoreErlang.Parser as P
import Language.Erlang.Lang
import Control.Exception (throw, SomeException, catch)
import Control.Monad.State (liftIO)

-- THESE ARE DUMMY TESTS TO BE REPLACED WITH SOME REAL ErlHask TESTS


main :: IO ()
main = defaultMainWithOpts
       [ testCase "rev" testRev
       , testCase "lambda" lambdaTest
       , testProperty "listRevRevId" propListRevRevId
       ] mempty

lambdaTestCode = [r|module 'boot' ['lambda_test'/0]
    attributes []
'lambda'/1 =
    fun (_cor0) ->
        ( fun (_cor3) ->
              let <_cor2> = call 'erlang':'+' (_cor0, 10)
              in call 'erlang':'+' (_cor2, _cor3)
          -| [{'id',{0,0,'-lambda/1-fun-0-'}}] )
'lambda_test'/0 =
    fun () ->
        let <L> = apply 'lambda'/1 (1)
        in apply L (5)
|]

lambdaTest :: Assertion
lambdaTest = undefined

testInProc :: String -> String -> Assertion
testInProc modsrc fn = do

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

  let Right m = P.parseModule modsrc
      m' = EModule "tested" (unann m)
      mt = M.insert "tested" m' newBaseModTable
  mmt <- newMVar mt
  let runner = applyMFA "tested" fn []
  let ev = do
        result <- runErlProcess runner bootModule mmt newProcDict
        return ()
  catch ev (\(e :: SomeException) -> do
                 liftIO $ print (show e)
                 throw e)

testRev :: Assertion
testRev = reverse [1, 2, 3] @?= [3, 2, 1]

propListRevRevId :: [Int] -> Property
propListRevRevId xs = not (null xs) ==> reverse (reverse xs) == xs
