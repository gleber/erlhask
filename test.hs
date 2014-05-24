{-# LANGUAGE DeriveGeneric, StandaloneDeriving, DeriveDataTypeable,
FlexibleInstances, RankNTypes, FlexibleContexts, ImpredicativeTypes #-}

-- import ErlCore

-- import ErlBifErlang

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Identity
import Control.Monad.Error (ErrorT, Error, runErrorT, throwError, strMsg)
import Control.Monad.RWS
import qualified Data.Map as M

data ErlTerm = ErlAtom String |
               ErlNum Integer |
               ErlFloat Double
             deriving (Show, Eq)

type ModName = String
type FunName = String
type ErlArity = Integer
type Key = String

type ErlMFA = (ModName, FunName, ErlArity)

data StackFrame = Frame { mfa :: ErlMFA,
                          args :: [ErlTerm] }

data ErlExceptionType = ExcError String |
                        ExcThrow |
                        ExcExit

data ErlException = ErlException { exc_type :: ErlExceptionType,
                                   frame :: StackFrame }
instance Error ErlException where
  strMsg _ = ErlException { }

type ErlProcessState m = RWST [StackFrame] [String] () m
type ErlProcessEvaluator m = ErrorT ErlException (ErlProcessState m)
type ErlProcess = ErlProcessEvaluator IO

runProcess :: ErlProcess ErlTerm -> IO (Either ErlException ErlTerm)
runProcess p = do
  (res, _, _) <- runRWST (runErrorT p) [Frame {}] ()
  return res

type ErlPure = ErlProcessEvaluator Identity

runPure :: ErlPure ErlTerm -> Either ErlException ErlTerm
runPure p =
  let (res, _, _) = runIdentity $ runRWST (runErrorT p) [Frame {}] ()
  in res

type ErlGeneric = Monad m => ErlProcessEvaluator m ErlTerm
type ErlStdFun = ([ErlTerm] -> ErlProcess ErlTerm)
type ErlPureFun = ([ErlTerm] -> ErlGeneric)


data ErlFun = ErlStdFun ErlStdFun |
              ErlPureFun ErlPureFun

bif_badarg :: ErlGeneric
bif_badarg = throwError (ErlException {})

erlang_display :: ErlStdFun
erlang_display (arg:[]) = do
  liftIO $ print arg
  return arg
erlang_display _ = bif_badarg

erlang_plus :: ErlPureFun
erlang_plus (a:b:[]) =
  case (a, b) of
    (ErlNum aa, ErlNum bb) -> return $ ErlNum (aa + bb)
    _ -> bif_badarg

apply :: ErlFun -> [ErlTerm] -> ErlProcess ErlTerm
apply (ErlStdFun f) a = f a
apply (ErlPureFun f) a = f a

eval :: ErlProcess ErlTerm
eval = do
  res <- apply (ErlPureFun erlang_plus) [(ErlNum 1), (ErlNum 42)]
  apply (ErlStdFun erlang_display) [res]

-- safeEval :: ErlPure ErlTerm
-- safeEval = do
--   erlang_plus [(ErlNum 1), (ErlNum 42)]

-- main :: IO ()
-- main = do
--   (res, s) <- runRWST (eval) 666
--   print "Result:"
--   print res
--   print "State:"
--   print s
