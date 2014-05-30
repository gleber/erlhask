{-# LANGUAGE DeriveGeneric, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances, Rank2Types, FlexibleContexts #-}

module ErlBifsCommon where

import ErlCore
import Control.Monad.Error (throwError)
import Control.Monad.RWS (ask)


bif_badarg :: String -> ErlGeneric ErlTerm
bif_badarg s = do
  stack <- ask
  throwError (ErlException { exc_type = ExcError,
                             reason = ErlAtom s,
                             stack = stack })

bif_badarg_num :: ErlGeneric ErlTerm
bif_badarg_num = bif_badarg "badarity"

bif_badarg_t :: ErlGeneric ErlTerm
bif_badarg_t = bif_badarg "badtype"
