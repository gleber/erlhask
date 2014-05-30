{-# LANGUAGE DeriveGeneric, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances, Rank2Types, FlexibleContexts #-}

module ErlBifsCommon where

import ErlCore
import Control.Monad.Error (throwError)

import Control.Monad.Trans.Class (lift)

bif_badarg :: ErlGeneric ErlTerm
bif_badarg = throwError (ErlException {})

bif_badarg_num :: ErlGeneric ErlTerm
bif_badarg_num = bif_badarg

bif_badarg_t :: ErlGeneric ErlTerm
bif_badarg_t = bif_badarg
