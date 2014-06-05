{-# LANGUAGE QuasiQuotes #-}

import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Text.RawString.QQ

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

testInProc :: String -> String -> Assertion
testInProc = undefined

testRev :: Assertion
testRev = reverse [1, 2, 3] @?= [3, 2, 1]

propListRevRevId :: [Int] -> Property
propListRevRevId xs = not (null xs) ==> reverse (reverse xs) == xs
