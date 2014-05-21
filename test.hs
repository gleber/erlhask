{-# LANGUAGE DeriveGeneric, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances, RankNTypes, FlexibleContexts #-}

-- import ErlCore

-- import ErlBifErlang

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, runStateT)
import qualified Data.Map as M

data ErlTerm = ErlAtom String |
               ErlNum Integer |
               ErlFloat Double
             deriving (Show, Eq)

type ModName = String

type BaseErlProcessState m a = Monad m => StateT Int m a

type ErlProcessState a = BaseErlProcessState IO a
type ErlPureState a = BaseErlProcessState Maybe a

type ErlStdFun = ([ErlTerm] -> ErlProcessState ErlTerm)
type ErlPureFun = Monad m => ([ErlTerm] -> BaseErlProcessState m ErlTerm)

data ErlFun = ErlStdFun ErlStdFun |
              ErlPureFun ErlPureFun

bif_badarg :: String -> BaseErlProcessState m a
bif_badarg a = fail a

erlang_display :: ErlStdFun
erlang_display (arg:[]) = do
  liftIO $ print arg
  return arg
erlang_display _ = bif_badarg "num"

erlang_plus :: ErlPureFun
erlang_plus (a:b:[]) =
  case (a, b) of
    (ErlNum aa, ErlNum bb) -> return $ ErlNum (aa + bb)
    _ -> bif_badarg "type"

apply :: ErlFun -> [ErlTerm] -> ErlProcessState ErlTerm
apply (ErlStdFun f) a = f a
apply (ErlPureFun f) a = f a

eval :: ErlProcessState ErlTerm
eval = do
  res <- apply (ErlPureFun erlang_plus) [(ErlNum 1), (ErlNum 42)]
  let (Just (res2, _)) = runStateT safeEval 99
  res3 <- erlang_plus [res, res2]
  apply (ErlStdFun erlang_display) [res3]

safeEval :: ErlPureState ErlTerm
safeEval = do
  erlang_plus [(ErlNum 1), (ErlNum 42)]

main :: IO ()
main = do
  (res, s) <- runStateT (eval) 666
  print "Result:"
  print res
  print "State:"
  print s
