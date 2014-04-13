{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}
module Proble where

import Control.Distributed.Process
import Control.Distributed.Process.Closure

import Control.DeepSeq (NFData)

import Data.Binary
import Data.Typeable (Typeable)
import GHC.Generics

data ErlTerm = ErlTerm
             deriving (Typeable, Generic, Eq)

instance Binary ErlTerm where
instance NFData ErlTerm where

evaluator :: ErlTerm -> Process ()
evaluator _term =
  return ()

$(remotable ['evaluator])

-- main = do
--   print $ put ErlTerm

main = undefined
