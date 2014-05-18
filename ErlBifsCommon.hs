{-# LANGUAGE DeriveGeneric, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances, Rank2Types, FlexibleContexts #-}

module ErlBifsCommon where

import ErlCore

import Control.Monad.Trans.Class (lift)

bif_badarg :: String -> BaseErlProcessState m a
bif_badarg str = lift $ fail ("badarg<" ++ str ++ ">")
-- bif_badarg str = error ("badarg<" ++ (show threadId) ++ "," ++ str ++ ">")

bif_badarg_num :: (Monad m) => BaseErlProcessState m a
bif_badarg_num = bif_badarg "wrong-number"

bif_badarg_t :: (Monad m) => BaseErlProcessState m a
bif_badarg_t = bif_badarg "wrong-types"
