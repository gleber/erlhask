import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

-- THESE ARE DUMMY TESTS TO BE REPLACED WITH SOME REAL ErlHask TESTS

main :: IO ()
main = defaultMainWithOpts
       [ testCase "rev" testRev
       , testProperty "listRevRevId" propListRevRevId
       ] mempty

testRev :: Assertion
testRev = reverse [1, 2, 3] @?= [3, 2, 1]

propListRevRevId :: [Int] -> Property
propListRevRevId xs = not (null xs) ==> reverse (reverse xs) == xs
