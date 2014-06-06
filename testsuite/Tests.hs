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
import Control.Exception (throw, SomeException)
import Control.Monad.State (liftIO)

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Network.Transport.TCP


main :: IO ()
main = defaultMainWithOpts
       [ testCase "rev" testRev
       , testCase "lambda" lambdaTest
       , testProperty "listRevRevId" propListRevRevId
       ] mempty

lambdaTestCode = [r|
module 'lambda_test' ['lambda_test'/0]
    attributes []
'lambda_test'/0 =
    fun () ->
        let <L> =
            ( fun (_cor1) ->
                  call 'erlang':'+'
                      (1, _cor1)
              -| [{'id',{0,0,'-lambda_test/0-fun-0-'}}] )
        in
            apply L
                (5)
end
|]

lambdaTest :: Assertion
lambdaTest = testInProc lambdaTestCode "lambda_test" (ErlNum 6)

testProc :: String -> String -> MVar ErlTerm -> Process ()
testProc modsrc fn res = do
  let Right m = P.parseModule modsrc
      m' = EModule "tested" (unann m)
      mt = M.insert "tested" m' newBaseModTable
  mmt <- liftIO $ newMVar mt
  let runner = applyMFA "tested" fn []
  let ev = do
        Right term <- runErlProcess runner bootModule mmt newProcDict
        liftIO $ putMVar res term
        return ()
  catch ev (\(e :: SomeException) -> do
                 liftIO $ print (show e)
                 throw e)

testInProc :: String -> String -> ErlTerm -> Assertion
testInProc modsrc fn expected = do
  Right tr <- createTransport "localhost" "8999" defaultTCPParameters
  node <- newLocalNode tr initRemoteTable
  res <- newEmptyMVar
  runProcess node (testProc modsrc fn res)
  result <- takeMVar res
  result @?= expected

-- TESTS BELOW ARE DUMMY TESTS TO BE REPLACED WITH SOME REAL ErlHask TESTS
testRev :: Assertion
testRev = reverse [1, 2, 3] @?= [3, 2, 1]

propListRevRevId :: [Int] -> Property
propListRevRevId xs = not (null xs) ==> reverse (reverse xs) == xs
