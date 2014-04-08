{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveDataTypeable #-}
module Proble where

import Control.Distributed.Process
import Control.Distributed.Process.Closure

import Data.Binary (Binary)
import Data.Typeable
import GHC.Generics

data ErlTerm = ErlFloat Double
             deriving (Generic, Typeable)

instance Binary ErlTerm

evaluator :: ErlTerm -> Process ()
evaluator _term =
  return ()

remotable ['evaluator]

main = undefined
