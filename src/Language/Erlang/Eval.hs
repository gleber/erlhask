{-# LANGUAGE TemplateHaskell, DataKinds, DeriveGeneric, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances, RankNTypes, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Erlang.Eval where

import Debug.HTrace

import Control.Distributed.Process
import Control.Distributed.Process.Closure
-- import Control.Distributed.Process.Node
-- import Network.Transport.TCP

import Data.Hashable
-- import Data.Either.Utils
import Data.Binary
import Data.Binary.Put

import Control.Exception (throw, SomeException)

import qualified Data.Map as M
import qualified Data.List as L
import Data.ByteString.Char8 ()
import qualified Data.ByteString.Lazy as BS

import Control.Monad.State (
  get,
  modify,
  evalStateT)

import Control.Monad.RWS (gets, ask)

import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Error (throwError, catchError)
import Control.Concurrent.MVar

import Language.CoreErlang.Syntax as S

import Language.Erlang.Core
import Language.Erlang.Util
import Language.Erlang.Modules
import Language.Erlang.Lang
import Language.Erlang.Bifs as Bifs
import Language.Erlang.Bif.Erlang as Erlang
import Language.Erlang.BifsCommon as BifsCommon
import qualified Language.Erlang.SafeEval as Safe

exprListToTerm :: EvalCtx ->  S.List S.Exps -> ErlProcess ErlTerm
exprListToTerm eCtx (LL exprs x) = do
  vals <- mapM (uevalExps eCtx) exprs
  ErlList t <- uevalExps eCtx x
  return $ ErlList (vals ++ t)
exprListToTerm eCtx (L exprs) = do
  vals <- mapM (uevalExps eCtx) exprs
  return $ ErlList vals

-- PList (LL [PVar "K"] (PList (LL [PVar "L"] (PVar "_cor8")))) [1, 2, 3]

uevalExps :: EvalCtx -> S.Exps -> ErlProcess ErlTerm
uevalExps a b = unseqM $ (evalExps a b)

evalExps :: EvalCtx -> S.Exps -> ErlProcess ErlSeq
evalExps eCtx (Exp e) = do
  term <- eval eCtx (unann e)
  return $ ErlSeq [term]
evalExps eCtx (Exps aexs) = do
  let exs = unann aexs
      xs = L.map unann exs
  l <- mapM (eval eCtx) xs
  return $ ErlSeq l


eval :: EvalCtx -> S.Exp -> ErlProcess ErlTerm
-- eval _ expr | htrace (show threadId ++ ": eval " ++ show expr) False = undefined
eval eCtx (Seq a b) = do
  evalExps eCtx a
  uevalExps eCtx b

eval _ (Lit l) = return $ literalToTerm l
eval eCtx (Tuple exps) = do
  elements <- mapM (uevalExps eCtx) exps
  return $ ErlTuple elements
eval eCtx (Let (var,val) exps) = do
  value <- evalExps eCtx val
  let eCtx' = setupFunctionContext eCtx (var,value)
  uevalExps eCtx' exps

eval (ECtx varTable) (Var var) = do
  case M.lookup var varTable of
    Just val -> return val
    Nothing -> dieL ["Missing binding?", var, show $ M.toList varTable]

eval eCtx (ModCall (mod0, arity0) args0) = do
  let uevalExps' = uevalExps eCtx
  emod <- uevalExps' mod0
  arity <- uevalExps' arity0
  args <- mapM uevalExps' args0
  modCall emod arity args

eval eCtx (App lambda args) = do
  cmod <- getCurrentModule
  -- () <- htrace ("Module on App: " ++ (show cmod)) $ return ()
  let uevalExps' = uevalExps eCtx
      modName = mod_name cmod
      Just modSource = source cmod
  lambda' <- uevalExps' lambda
  args' <- mapM uevalExps' args
  case lambda' of
    (ErlFunName name arity) -> do
      let frame = Frame { mfa = (modName, name, arity),
                          args = Nothing,
                          pos = FilePos "" 0 }
      withFrame frame $ evalModFn modSource name args'
    fun@(ErlLambda name argNames _eCtx _exprs) -> do
      let frame = Frame { mfa = (modName, name, toInteger $ length argNames),
                          args = Nothing,
                          pos = FilePos "" 0 }
      withFrame frame $ applyFunLambda fun args'
    _ ->
      bif_badarg_t --dieL ["Can not apply", show lambda']

eval _ (Fun (Function ((Atom name), arity))) =
  return $ ErlFunName name arity

eval eCtx (Lambda argNames exprs) = do
  return $ ErlLambda (show $ hash exprs) argNames eCtx exprs

eval eCtx (Case val alts) = do
  let alts' = map unann alts
  val' <- evalExps eCtx val
  matchAlts eCtx alts' val'

eval eCtx (List list) = do
  exprListToTerm eCtx list

eval eCtx (Op (Atom op) args) = do
  let uevalExps' = uevalExps eCtx
  args' <- mapM uevalExps' args
  case op of
    "match_fail" -> dieL $ map show args'
    _ -> errorL ["Not implemented Op", op, show args]

eval eCtx (Rec alts (TimeOut time timeoutExps)) = do
  time' <- uevalExps eCtx time
  case isTimeout time' of
    False ->
      BifsCommon.bif_badarg_t
    True -> do
      let alts' = map unann alts
      matches <- receiveMatches eCtx alts'
      res <- lift $ lift $ receive time' matches
      case res of
        Nothing ->
          uevalExps eCtx timeoutExps
        Just (msg, (Alt pats _ exprs)) -> do
          let Just eCtx' = matchPats eCtx pats (ErlSeq [msg])
          uevalExps eCtx' exprs

eval eCtx (Try body (bodyBind, success) (catchVars, catchBody)) = do
  cm <- getCurrentModule
  pd <- gets proc_dict
  let catcher = (\exc@(ErlException { exc_type = excType, reason = term }) -> do
                    let vars = (L.map (: []) catchVars)
                        seq = [ErlSeq [excTypeToTerm(excType)],
                               ErlSeq [term],
                               ErlSeq [ErlList []]]
                        eCtx' = setupFunctionContext' eCtx (L.zip vars seq)
                    modify $ \ps ->
                      ps { last_exc = Just exc }
                    evalExps eCtx' catchBody
                )
  val <- (evalExps eCtx body) `catchError` catcher
  let eCtx' = setupFunctionContext eCtx (bodyBind, val)
  uevalExps eCtx' success

eval eCtx (Catch body) = do
  let catcher = (\exc@(ErlException { exc_type = excType, reason = term }) -> do
                    modify $ \ps ->
                      ps { last_exc = Just exc }
                    let x = ErlTuple $ [term, stacktraceToTerm (stacktrace exc)]
                    case excType of
                      ExcThrow ->
                        return $ x
                      _ ->
                        return $ ErlTuple $ [excTypeToTerm(excType), x]
                )
  (uevalExps eCtx body) `catchError` catcher

eval eCtx (Binary bs) = do
  bs' <- mapM (evalBitString eCtx) bs
  return $ ErlBinary $ runPut $ mapM_ putLazyByteString bs'

eval _ expr =
  errorL ["Unhandled expression: ", show expr]

evalBitString :: EvalCtx -> S.BitString Exps -> ErlProcess BS.ByteString
evalBitString eCtx (BitString e params) = do
  ErlNum v <- uevalExps eCtx e
  pars <- mapM (uevalExps eCtx) params
  -- htrace ("Bin build params: " ++ (show pars)) $
  let ((ErlNum 1):(ErlNum 8):_) = pars
  return $ runPut $ putWord8 $ fromInteger v

--
-- RECEIVE
--

receiveMatches :: EvalCtx -> [S.Alt] -> ErlProcess [Match (ErlTerm, S.Alt)]
receiveMatches eCtx0 alts = do
  mt <- getModTable
  return $ map (\alt@(Alt pats guard exprs) ->
    (matchIf (\(msg :: ErlTerm) ->
               let matched = matchPats eCtx0 pats (ErlSeq [msg])
               in
                case matched of
                  Just eCtx -> do
                    Safe.matchGuard eCtx mt guard
                  Nothing ->
                    False
             )
             (\(msg :: ErlTerm) -> do
                 return (msg, alt)
             )) :: Match (ErlTerm, S.Alt)) alts

--
-- CASES AND PATTERNS
--

matchAlts :: EvalCtx -> [S.Alt] -> ErlSeq -> ErlProcess ErlTerm
matchAlts _ [] _ = dieL ["No matching clauses"]
matchAlts eCtx0 (alt:xs) seq = do
  let (Alt pats guard exprs) = alt
  let matched = matchPats eCtx0 pats seq
  case matched of
    Just eCtx -> do
      guarded <- matchGuard eCtx guard
      case guarded of
        True ->
          uevalExps eCtx exprs
        False ->
          matchAlts eCtx xs seq
    Nothing ->
      matchAlts eCtx0 xs seq

matchPats :: EvalCtx -> S.Pats -> ErlSeq -> Maybe EvalCtx
matchPats eCtx (Pats pats) (ErlSeq seq) =
  foldM (\e (p,t) -> matchPat e (unann p) t) eCtx (L.zip pats seq)

matchPats _eCtx pat seq =
  errorL ["matchPats: Not implemented matching of", show pat, "with", show seq]

matchPat :: EvalCtx -> S.Pat -> ErlTerm -> Maybe EvalCtx
matchPat eCtx (PTuple pat) (ErlTuple term) = do
  if
    L.length pat == L.length term
    then foldM (\ctx (p,e) -> matchPat ctx p e) eCtx (L.zip pat term)
    else Nothing
matchPat eCtx (PTuple pat) _ = Nothing

matchPat eCtx (PBinary (p:ps)) (ErlBinary bs) | BS.length bs >= 1 = do
  let BitString pp params = p
      mt = newPureModTable
  let Right pars = mapM (runErlPure mt . Safe.uevalExps eCtx) params
  let ((ErlNum 1):(ErlNum 8):_) = htrace ("Bin build params: " ++ (show pars)) $ pars
  -- pp :: Hole
  eCtx' <- matchPat eCtx pp (ErlNum $ toInteger $ BS.head bs) --HACK: assumes that we are matching bytes only
  matchPat eCtx' (PBinary ps) (ErlBinary $ BS.tail bs)
matchPat eCtx (PBinary []) (ErlBinary _) = Just eCtx
matchPat _eCtx (PBinary _) _ = Nothing

matchPat eCtx (PList (LL [h] t)) (ErlList (x:xs)) = do
  eCtx' <- matchPat eCtx h x
  matchPat eCtx' t (ErlList xs)
matchPat eCtx (PList _) _ = Nothing

matchPat eCtx (PLit lit) term = do
  let lit' = literalToTerm lit
  if lit' == term
    then Just eCtx
    else Nothing
matchPat eCtx (PVar var) term = do
  return $ setupFunctionContext eCtx ([var], ErlSeq [term])


matchPat eCtx (PAlias (Alias var pat)) term = do
  eCtx' <- matchPat eCtx pat term
  return $ setupFunctionContext eCtx ([var], ErlSeq [term])

matchPat _eCtx _pat _term = do
  errorL ["matchPat: Not implemented matching of", show _pat, "with", show _term]

matchGuard :: EvalCtx -> S.Guard -> ErlProcess Bool
matchGuard eCtx (Guard exprs) = do
  evaled <- uevalExps eCtx exprs
  case evaled of
    ErlAtom "true" ->
      return True
    _ -> return False

--
-- FUNCTIONS AND APPLICATIONS
--
-- Function-like things in Erlang:
-- * Expressions:
--   * F(A..) = CoreErlang::Apply
--   * M:F(A..) = CoreErlang::ModCall
-- * Values
--   * fun m:f/N = ErlTerm::ErlFunName
--   * fun (A..) -> ... end. = ErlTerm::ErlLambda
-- * Modules
--   * exression-defined function = ErlCoreFun
--   * sidefull BIF = ErlStdFun
--   * pure BIF = ErlPureFun

modCall :: ErlTerm -> ErlTerm -> [ErlTerm] -> ErlProcess ErlTerm
modCall (ErlAtom emod) (ErlAtom fn) args =
  applyMFA emod fn args

modCall emod fn args =
  --TODO: what about abstract modules?
  dieL ["Wrong type of call", show emod, show fn, show args]

applyMFA :: ModName -> FunName -> [ErlTerm] -> ErlProcess ErlTerm
-- applyMFA modname fn args | htrace ("applyMFA " ++ modname ++ ":" ++ fn) $ False = undefined
applyMFA modname fn args = do
  erlmod <- ensureModule modname
  let arity = (toInteger (length args))
      modName = mod_name erlmod
      frame = Frame { mfa = (modName, fn, arity),
                      args = Nothing,
                      pos = FilePos "" 0 }
      fun = M.lookup (fn, arity) (funs erlmod) `forceMaybeL` ["Function not found for:", showFunCall modName fn args]
  withModule erlmod $ applyFun fun args

applyFunLambda :: ErlTerm -> [ErlTerm] -> ErlProcess ErlTerm
applyFunLambda (ErlLambda _name names ctx exprs) args =
  let fun = expsToErlFun ctx names exprs
  in applyFun fun args
applyFunLambda _ _ =
  errorL ["Wrong type for applyFunLambda call, dummy!"]

applyFun :: ErlFun -> [ErlTerm] -> ErlProcess ErlTerm
applyFun (ErlCoreFun vars exps) args = applyFun fun args
  where fun = expsToErlFun newEvalCtx vars exps
applyFun (ErlStdFun fun) args = applyStdFun fun args
applyFun (ErlPureFun fun) args = applyPureFun fun args

applyStdFun :: ErlStdFun -> [ErlTerm] -> ErlProcess ErlTerm
applyStdFun fun args = fun args

applyPureFun :: ErlPureFun -> [ErlTerm] -> ErlGeneric ErlTerm
applyPureFun fun args = fun args

expsToErlFun :: EvalCtx -> [Var] -> S.Exps -> ErlFun
expsToErlFun eCtx argNames expressions =
  let arity = length argNames
      fn = \args -> do
        case length args of
          a | a == arity ->
            let
              bargs = L.zipWith (\x y -> ([x], ErlSeq [y])) argNames args
              evalCtx' = setupFunctionContext' eCtx bargs
            in
             uevalExps evalCtx' expressions
          -- _ ->
          --   BifsCommon.bif_badarg_num
  in
   ErlStdFun fn

evalModFn :: S.Module -> String -> [ErlTerm] -> ErlProcess ErlTerm
evalModFn emod fn args = do
  let arity = (toInteger (length args))
    in case findModFn emod fn arity of
    Just fun ->
      applyFunLambda fun args
    Nothing ->
      dieL ["Can not find", showShortFunName fn arity, "in", show emod]

erlang_apply :: ErlStdFun
erlang_apply [lambda, ErlList args] = do
  cmod <- getPreviousModule
  let Just coreMod = source cmod
      mn = mod_name cmod
  case lambda of
    (ErlFunName name arity) -> do
      let frame = Frame { mfa = (mn, name, arity),
                          args = Just $ [lambda, ErlList args],
                          pos = FilePos "" 0 }
      popModule $ withFrame frame $ evalModFn coreMod name args
    fun@(ErlLambda name _argNames _eCtx _exprs) -> do
      let arity = (toInteger (length args))
      let frame = Frame { mfa = (mn, name, arity),
                          args = Just $ [lambda, ErlList args],
                          pos = FilePos "" 0 }
      popModule $ withFrame frame $ applyFunLambda fun args
    _ -> do
      liftIO $ putStrLn "erlang:apply badarg type"
      BifsCommon.bif_badarg_t
erlang_apply _ = bif_badarg_num

erlang_spawn :: ErlStdFun
erlang_spawn [lambda] = do
  Left mmt <- gets mod_table
  cmod <- getPreviousModule
  pid <- lift $ lift $ spawnLocal $ localEvaluator cmod mmt ("erlang", "apply", [lambda, ErlList []])
  return $ ErlPid pid
erlang_spawn _ = bif_badarg_num

erlang_spawn_link :: ErlStdFun
erlang_spawn_link [lambda] = do
  p <- erlang_spawn [lambda]
  Erlang.erlang_link [p]
  return $ p
erlang_spawn_link _ = bif_badarg_num

preloadedModuleNames :: [ModName]
preloadedModuleNames = ["erlang", "erl_prim_loader", "init", "otp_ring0",
                        "prim_file", "prim_inet", "prim_zip", "zlib"]

newPureModTable :: ModTable
newPureModTable = do
  let e = M.singleton "erlang" erlangEvalMod
      bifModules = Bifs.newBifsModTable
  M.unionsWith merge [e, bifModules]

newBaseModTable :: IO ModTable
newBaseModTable = do
  cores <- mapM loadCoreModule preloadedModuleNames
  let Right cores' = sequence cores
  let pure = newPureModTable
      bifModules = Bifs.newBifsModTable
  return $ M.unionsWith merge [pure, bifModules]

erlangEvalMod :: ErlModule
erlangEvalMod =
  ErlModule { mod_name = "erlang",
              source = Nothing,
              funs = evalBifs }

evalBifs :: ErlFunTable
evalBifs =
  M.fromList [
    (("apply", 2), ErlStdFun erlang_apply),
    (("spawn_link", 1), ErlStdFun erlang_spawn_link),
    (("spawn", 1), ErlStdFun erlang_spawn)]

-- TODO: remote evaluator can not accept MVar, so instead it should
-- ask boot/kernel/init process for MVar ModTable (does not work,
-- since local coordinator can not send me a message); so instead I'd
-- have to send a message to coordinator on remote node, which would
-- 'spawnLocal' actual process and supply it MVar ModTable
localRunner :: ErlModule -> MVar ModTable -> ErlProcess ErlTerm -> Process ()
localRunner cmod mmt runner = do
  let ev = do
        r <- runErlProcess runner cmod mmt newProcDict
        case r of
          Left e -> do
            say $ show e
            die e
          Right _ ->
            return ()
  catch ev (\(e :: SomeException) -> do
                 liftIO $ print (show e)
                 throw e)

localEvaluator :: ErlModule -> MVar ModTable -> (ModName, FunName, [ErlTerm]) -> Process ()
localEvaluator cmod mmt (emod, fn, args) = do
  let runner = applyMFA emod fn args
  localRunner cmod mmt runner
