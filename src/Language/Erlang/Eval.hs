{-# LANGUAGE TemplateHaskell, DataKinds, DeriveGeneric, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances, RankNTypes, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ErlEval where

import Debug.HTrace

import Control.Distributed.Process
import Control.Distributed.Process.Closure
-- import Control.Distributed.Process.Node
-- import Network.Transport.TCP

import Data.Hashable
-- import Data.Either.Utils


import Control.Exception (throw, SomeException)

import qualified Data.Map as M
import qualified Data.List as L

import Control.Monad.State (
  get,
  modify,
  evalStateT)

import Control.Monad.RWS (gets)

import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Error (throwError, catchError)

-- import Control.Exception.Lifted

import Language.CoreErlang.Syntax as S

import ErlCore
import ErlUtil
import ErlModules
import ErlLangCore
import ErlBifs as Bifs
import ErlBifsCommon as BifsCommon
import qualified ErlSafeEval as Safe

exprListToTerm :: EvalCtx ->  S.List S.Exps -> ErlProcess ErlTerm
exprListToTerm eCtx (LL exprs xs) = do
  vals <- mapM (evalExps eCtx) exprs
  ErlList t <- evalExps eCtx xs
  return $ ErlList (vals ++ t)
exprListToTerm eCtx (L exprs) = do
  vals <- mapM (evalExps eCtx) exprs
  return $ ErlList vals

-- PList (LL [PVar "K"] (PList (LL [PVar "L"] (PVar "_cor8")))) [1, 2, 3]

evalExps :: EvalCtx -> S.Exps -> ErlProcess ErlTerm
evalExps eCtx (Exp e) = eval eCtx (unann e)
evalExps eCtx (Exps aexs) = do
  let exs = unann aexs
      xs = L.map unann exs
  fmap last $ mapM (eval eCtx) xs

eval :: EvalCtx -> S.Exp -> ErlProcess ErlTerm
-- eval _ expr | htrace (show threadId ++ ": eval " ++ show expr) False = undefined
eval eCtx (Seq a b) = do
  _ <- evalExps eCtx a
  evalExps eCtx b

eval _ (Lit l) = return $ literalToTerm l
eval eCtx (Tuple exps) = do
  elements <- mapM (evalExps eCtx) exps
  return $ ErlTuple elements
eval eCtx (Let (var,val) exps) = do
  value <- evalExps eCtx val
  let eCtx' = setupFunctionContext eCtx (var,value)
  evalExps eCtx' exps

eval eCtx (ModCall (mod0, arity0) args0) = do
  let evalExps' = evalExps eCtx
  emod <- evalExps' mod0
  arity <- evalExps' arity0
  args <- mapM evalExps' args0
  modCall emod arity args

eval (ECtx varTable) (Var var) = do
  case M.lookup var varTable of
    Just val -> return val
    Nothing -> dieL ["Missing binding?", var, show $ M.toList varTable]

eval eCtx (App lambda args) = do
  cmod <- gets curr_mod
  case cmod of
    EModule _ curMod -> do
      let evalExps' = evalExps eCtx
      lambda' <- evalExps' lambda
      args' <- mapM evalExps' args
      case lambda' of
        (ErlFunName name _arity) -> do
          evalModFn curMod name args'
        fun@(ErlLambda _name _argNames _eCtx _exprs) -> do
          applyFunLambda fun args'
        _ ->
          dieL ["Can not apply", show lambda']
    _ ->
      errorL ["Unable to apply things in ", show cmod]

eval _ (Fun (Function ((Atom name), arity))) =
  return $ ErlFunName name arity

eval eCtx (Lambda argNames exprs) = do
  return $ ErlLambda (show $ hash exprs) argNames eCtx exprs

eval eCtx (Case val alts) = do
  let alts' = map unann alts
  val' <- evalExps eCtx val
  matchAlts eCtx val' alts'

eval eCtx (List list) = do
  exprListToTerm eCtx list

eval eCtx (Op (Atom op) args) = do
  let evalExps' = evalExps eCtx
  args' <- mapM evalExps' args
  case op of
    "match_fail" -> dieL $ map show args'
    _ -> errorL ["Not implemented Op", op, show args]

eval eCtx (Rec alts (TimeOut time timeoutExps)) = do
  time' <- evalExps eCtx time
  case isTimeout time' of
    False ->
      BifsCommon.bif_badarg_t
    True -> do
      let alts' = map unann alts
      matches <- receiveMatches eCtx alts'
      res <- lift $ lift $ receive time' matches
      case res of
        Nothing ->
          evalExps eCtx timeoutExps
        Just (msg, (Alt pats _ exprs)) -> do
          let Just eCtx' = matchPats eCtx pats msg
          evalExps eCtx' exprs

eval eCtx (Try body (bodyBind, success) (catchVars, catchBody)) = do
  cm <- gets curr_mod
  mt <- gets mod_table
  pd <- gets proc_dict
  let catcher = (\exc@(ErlException { exc_type = excType, reason = term }) -> do
                  let eCtx' = setupFunctionContext' eCtx (L.zip (L.map (: []) catchVars) [excTypeToTerm(excType),
                                                                                          term,
                                                                                          ErlList []])
                  modify $ \ps ->
                    ps { last_exc = exc }
                  evalExps eCtx' catchBody
                )
  val <- (evalExps eCtx body) `catchError` catcher
  let eCtx' = setupFunctionContext eCtx (bodyBind, val)
  evalExps eCtx' success

eval _ expr =
  errorL ["Unhandled expression: ", show expr]


receiveMatches :: EvalCtx -> [S.Alt] -> ErlProcess [Match (ErlTerm, S.Alt)]
receiveMatches eCtx0 alts = do
  mt <- gets mod_table
  return $ map (\alt@(Alt pats guard exprs) ->
    (matchIf (\(msg :: ErlTerm) ->
               let matched = matchPats eCtx0 pats msg
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

matchAlts :: EvalCtx -> ErlTerm -> [S.Alt] -> ErlProcess ErlTerm
matchAlts _ _ [] = dieL ["No matching clauses"]
matchAlts eCtx0 val (alt:xs) = do
  let (Alt pats guard exprs) = alt
  let matched = matchPats eCtx0 pats val
  case matched of
    Just eCtx -> do
      guarded <- matchGuard eCtx guard
      case guarded of
        True ->
          evalExps eCtx exprs
        False ->
          matchAlts eCtx val xs
    Nothing ->
      matchAlts eCtx0 val xs

-- (Pats [PTuple [PLit (LAtom (Atom "ok")),
--                PVar "Z"]])
matchPats :: EvalCtx -> S.Pats -> ErlTerm -> Maybe EvalCtx
matchPats eCtx (Pats [pat]) term = matchPats eCtx (Pat pat) term
matchPats eCtx (Pat pat) term = do
  matchPat eCtx pat term
matchPats _eCtx pat term =
  errorL ["Not implemented matching of Pats", show pat, "with", show term]

matchPat :: EvalCtx -> S.Pat -> ErlTerm -> Maybe EvalCtx
matchPat eCtx (PTuple pat) (ErlTuple term) = do
  if
    L.length pat == L.length term
    then foldM (\ctx (p,e) -> matchPat ctx p e) eCtx (L.zip pat term)
    else Nothing
matchPat eCtx (PLit lit) term = do
  let lit' = literalToTerm lit
  if lit' == term
    then Just eCtx
    else Nothing
matchPat eCtx (PVar var) term = do
  return $ setupFunctionContext eCtx ([var], term)
matchPat _eCtx pat term = do
  errorL ["Not implemented matching of Pat", show pat, "with", show term]

-- PList (LL [PVar "K"] (PList (LL [PVar "L"] (PVar "_cor8")))) [1, 2, 3]
matchPat eCtx (PList (LL [h] t)) (ErlList (x:xs)) = do
  eCtx' <- matchPat eCtx h x
  matchPat eCtx' t (ErlList xs)

-- matchPat _ pat term = errorL ["Not implemented matching of", show pat, show term]

matchGuard :: EvalCtx -> S.Guard -> ErlProcess Bool
matchGuard eCtx (Guard exprs) = do
  evaled <- evalExps eCtx exprs
  case evaled of
    ErlAtom "true" ->
      return True
    _ -> return False

modCall :: ErlTerm -> ErlTerm -> [ErlTerm] -> ErlProcess ErlTerm
modCall (ErlAtom emod) (ErlAtom fn) args =
  applyMFA emod fn args

modCall emod fn args =
  dieL ["Wrong type of call", show emod, show fn, show args]

applyMFA :: ModName -> FunName -> [ErlTerm] -> ErlProcess ErlTerm
applyMFA modname fn args = do
  erlmod <- ensureModule modname
  applyFunInMod erlmod fn args

applyFunInMod :: ErlModule -> FunName -> [ErlTerm] -> ErlProcess ErlTerm
applyFunInMod erlmod fn args = do
  let arity = (toInteger (length args))
  case erlmod of
    EModule _ emodule -> do
      -- evalModFn :: EvalCtx -> S.Module -> String -> ErlArity -> ErlProcess ErlTerm
      modify $ \ps ->
        ps { curr_mod = erlmod }
      evalModFn emodule fn args
    HModule modname funs -> do
      let fun = M.lookup (fn, arity) funs `forceMaybeL` ["Function not found for:", showFunCall modname fn args]
      applyFun fun args

applyFunLambda :: ErlTerm -> [ErlTerm] -> ErlProcess ErlTerm
applyFunLambda (ErlLambda _name names ctx exprs) args =
  applyELambda ctx exprs names args
applyFunLambda _ _ =
  errorL ["Wrong type for applyFunLambda call, dummy!"]

applyELambda :: EvalCtx -> S.Exps -> [Var] -> [ErlTerm] -> ErlProcess ErlTerm
applyELambda eCtx expressions names args =
  let fun = expsToErlFun eCtx names expressions
  in applyFun fun args

applyFun :: ErlFun -> [ErlTerm] -> ErlProcess ErlTerm
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
              bargs = L.zipWith (\x y -> ([x],y)) argNames args
              evalCtx' = setupFunctionContext' eCtx bargs
            in
             evalExps evalCtx' expressions
          _ ->
            BifsCommon.bif_badarg_num
  in
   ErlStdFun fn

findExportedFunction :: String -> ErlArity -> [FunDef] -> Maybe FunDef
findExportedFunction name arity funs =
  let
    test = \(FunDef nm _) ->
      let (Function ((Atom n), a)) = unann nm
      in (n == name) && (a == arity)
  in
   L.find test funs

findFn :: String -> ErlArity -> [FunDef] -> Maybe ErlTerm
findFn name arity funs = do
  (FunDef _ aexp) <- findExportedFunction name arity funs
  let (Lambda vars exprs) = unann aexp
  return $ ErlLambda (show $ hash exprs) vars newEvalCtx exprs

findModFn :: S.Module -> String -> ErlArity -> Maybe ErlTerm
findModFn m name arity =
  let Module _modName _exports _attributes funs = m
  in
   findFn name arity funs

evalModFn :: S.Module -> String -> [ErlTerm] -> ErlProcess ErlTerm
evalModFn emod fn args = do
  let arity = (toInteger (length args))
    in case findModFn emod fn arity of
    Just fun ->
      applyFunLambda fun args
    Nothing ->
      dieL ["Can not find", showShortFunName fn arity, "in", show emod]

erlang_apply :: ErlStdFun
erlang_apply (lambda:(ErlList args):[]) = do
  case lambda of
    (ErlFunName name _arity) -> do
      (EModule _ curMod) <- gets curr_mod
      evalModFn curMod name args
    fun@(ErlLambda _name _argNames _eCtx _exprs) -> do
      applyFunLambda fun args
    _ -> do
      liftIO $ putStrLn "erlang:apply badarg type"
      BifsCommon.bif_badarg_t
erlang_apply _ = bif_badarg_num

erlang_spawn :: ErlStdFun
erlang_spawn (lambda:[]) = do
  pid <- lift $ lift $ spawnLocal (evaluator ("erlang", "apply", [lambda, ErlList []]))
  return $ ErlPid pid
erlang_spawn _ = bif_badarg_num

newBaseModTable :: ModTable
newBaseModTable =
  M.adjust adjustErlangModule "erlang" Bifs.newBifsModTable

adjustErlangModule :: ErlModule -> ErlModule
adjustErlangModule (HModule "erlang" funs) =
  HModule "erlang" $ adjustErlangModule' funs
adjustErlangModule _ =
  errorL ["Wrong module, dummy!"]

evalBifs :: ErlFunTable
evalBifs =
  M.fromList [
    (("apply", 2), ErlStdFun erlang_apply),
    (("spawn", 1), ErlStdFun erlang_spawn)]

adjustErlangModule' :: ErlFunTable -> ErlFunTable
adjustErlangModule' funs =
  M.union evalBifs funs

evaluator :: (ModName, FunName, [ErlTerm]) -> Process ()
evaluator (emod, fn, args) = do
  let runner = applyMFA emod fn args
  let ev = do
        result <- runErlProcess runner bootModule newBaseModTable newProcDict
        return ()
  catch ev (\(e :: SomeException) -> do
                 liftIO $ print (show e)
                 throw e)
remotable ['evaluator]
