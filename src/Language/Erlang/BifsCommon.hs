{-# LANGUAGE DeriveGeneric, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances, Rank2Types, FlexibleContexts #-}

module Language.Erlang.BifsCommon where

import Language.Erlang.Core
import Control.Monad.Error (throwError)
import Control.Monad.RWS (ask)


bif_badarg :: String -> ErlGeneric a
bif_badarg s = do
  stack <- ask
  throwError (ErlException { exc_type = ExcError,
                             reason = ErlAtom s,
                             stack = stack })

bif_badarg_num :: ErlGeneric a
bif_badarg_num = bif_badarg "badarity"

bif_badarg_t :: ErlGeneric a
bif_badarg_t = bif_badarg "badtype"

bif_notimpl :: ErlGeneric a
bif_notimpl = bif_badarg "notimpl"

atom_true = ErlAtom "true"
atom_false = ErlAtom "false"
atom_ok = ErlAtom "ok"

erlToBool :: ErlTerm -> ErlGeneric Bool
erlToBool (ErlAtom "true") = return $ True
erlToBool (ErlAtom "false") = return $ False
erlToBool _ = bif_badarg_t

erlIsBool :: ErlTerm -> Bool
erlIsBool (ErlAtom "true") = True
erlIsBool (ErlAtom "false") = True
erlIsBool _ = False
