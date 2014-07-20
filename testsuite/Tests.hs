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
import Language.Erlang.Modules
import qualified Language.CoreErlang.Parser as P
import Language.Erlang.Lang
import Control.Exception (throw, SomeException)
import Control.Monad.State (liftIO)

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Network.Transport.Chan


main :: IO ()
main = defaultMainWithOpts
       [ testCase "rev" testRev
       , testCase "lambda" lambdaTest
       , testCase "binary" binaryTest
       , testCase "binaryPattern" binaryPatterTest
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

--TODO: add tests for bit-by-bit contruction

binaryTestCode = [r|
module 'binary_test' ['binary_test'/0]
    attributes []
'binary_test'/0 =
    fun () ->
        let <Y> =
            0
        in  let <B> =
                #{#<1>(8,1,'integer',['unsigned'|['big']]),
                  #<2>(8,1,'integer',['unsigned'|['big']]),
                  #<3>(8,1,'integer',['unsigned'|['big']]),
                  #<Y>(8,1,'integer',['unsigned'|['big']])}#
            in  let <L> =
                    call 'erlang':'binary_to_list'
                        (B)
                in
                    call 'lists':'sum'
                        (L)
end
|]
binaryTest :: Assertion
binaryTest = testInProc binaryTestCode "binary_test" (ErlNum 6)

--TODO: add tests for bit-by-bit matching

binaryPatterTestCode = [r|
module 'tested' ['binary_test'/0]
    attributes []
'binary_test'/0 =
    fun () ->
        let <Z> =
            42
        in
            case #{#<1>(8,1,'integer',['unsigned'|['big']]),
                   #<Z>(8,1,'integer',['unsigned'|['big']])}# of
              <#{#<1>(8,1,'integer',['unsigned'|['big']]),
                 #<Y>(8,1,'integer',['unsigned'|['big']])}#> when 'true' ->
                  Y
              ( <_cor1> when 'true' ->
                    primop 'match_fail'
                        ({'badmatch',_cor1})
                -| ['compiler_generated'] )
            end
end|]

binaryPatterTest :: Assertion
binaryPatterTest = testInProc binaryPatterTestCode "binary_test" (ErlNum 42)

testProc :: String -> String -> MVar ErlTerm -> Process ()
testProc modsrc fn res = do
  let Right m = parseCoreModule "tested" modsrc
      mt0 = newPureModTable
      mt = M.insert "tested" m mt0
  mmt <- liftIO $ newMVar mt
  let runner = applyMFA "tested" fn []
  let ev = do
        r <- runErlProcess runner bootModule mmt newProcDict
        case r of
          Right term -> do
            liftIO $ putMVar res term
            return ()
          Left e ->
            error (show e)
  ev `catch` \(e :: SomeException) -> do
    liftIO $ print (show e)
    throw e

testInProc :: String -> String -> ErlTerm -> Assertion
testInProc modsrc fn expected = do
  tr <- createTransport
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
