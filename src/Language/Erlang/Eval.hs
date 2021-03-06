{-# LANGUAGE TemplateHaskell, DataKinds, DeriveGeneric, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances, RankNTypes, FlexibleContexts #-}
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
import qualified Data.ByteString.Lazy as BS

import Control.Monad.State (
  get,
  modify,
  evalStateT)

import Control.Monad.RWS (gets, local)

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
eval _ expr | htrace (show threadId ++ ": eval " ++ show expr) False = undefined
eval eCtx (Seq a b) = do
  _ <- evalExps eCtx a
  uevalExps eCtx b

eval _ (Lit l) = return $ literalToTerm l
eval eCtx (Tuple exps) = do
  elements <- mapM (uevalExps eCtx) exps
  return $ ErlTuple elements
eval eCtx (Let (var,val) exps) = do
  value <- evalExps eCtx val
  let eCtx' = setupFunctionContext eCtx (var,value)
  uevalExps eCtx' exps

eval eCtx (ModCall (mod0, arity0) args0) = do
  let uevalExps' = uevalExps eCtx
  emod <- uevalExps' mod0
  arity <- uevalExps' arity0
  args <- mapM uevalExps' args0
  modCall emod arity args

eval (ECtx varTable) (Var var) = do
  case M.lookup var varTable of
    Just val -> return val
    Nothing -> dieL ["Missing binding?", var, show $ M.toList varTable]

eval eCtx (App lambda args) = do
  cmod <- gets curr_mod
  case cmod of
    EModule _ curMod -> do
      let uevalExps' = uevalExps eCtx
      lambda' <- uevalExps' lambda
      args' <- mapM uevalExps' args
      case lambda' of
        (ErlFunName name arity) -> do
          let frame = Frame { mfa = (modName cmod, name, arity),
                              args = Nothing,
                              pos = FilePos "" 0 }
          local (\s -> (frame:s)) $ evalModFn curMod name args'
        fun@(ErlLambda name argNames _eCtx _exprs) -> do
          let frame = Frame { mfa = (modName cmod, name, toInteger $ length argNames),
                              args = Nothing,
                              pos = FilePos "" 0 }
          local (\s -> (frame:s)) $ applyFunLambda fun args'
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
  cm <- gets curr_mod
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

-- Catch
--   (Exp
--    (Constr
--     (ModCall
--      (Exp
--       (Constr
--        (Lit
--         (LAtom
--          (Atom \"erlang\")))),Exp
--                 (Constr
--                 (Lit
--                 (LAtom
--                 (Atom \"apply\"))))) [Exp
--                 (Constr
--                 (Var \"F\")),Exp
--                 (Constr
--                 (Lit LNil))])))

eval eCtx (Catch body) = do
  let catcher = (\exc@(ErlException { exc_type = excType, reason = term }) -> do
                    modify $ \ps ->
                      ps { last_exc = Just exc }
                    let x = ErlTuple $ [term, stacktraceToTerm (stack exc)]
                    case excType of
                      ExcThrow ->
                        return $ x
                      _ ->
                        return $ ErlTuple $ [excTypeToTerm(excType), x]
                )
  (uevalExps eCtx body) `catchError` catcher

-- ga =
--   Binary [BitString
--           (Exp (Constr (Lit (LInt 1))))
--           [Exp (Constr (Lit (LInt 8))),
--            Exp (Constr (Lit (LInt 1))),
--            Exp (Constr (Lit (LAtom (Atom "integer")))),
--            Exp (Constr (List (LL [Exp (Constr (Lit (LAtom (Atom "unsigned"))))]
--                               (Exp (Constr (List (L [Exp (Constr (Lit (LAtom (Atom "big"))))])))))))],
--           BitString (
--             Exp (
--                Constr (
--                   Lit (
--                      LInt 2)))) [Exp (
--                                     Constr (
--                                        Lit (
--                                           LInt 8))),
--                                  Exp (
--                                    Constr (
--                                       Lit (
--                                          LInt 1))),
--                                  Exp (
--                                    Constr (
--                                       Lit (
--                                          LAtom (
--                                             Atom "integer")))),
--                                  Exp (
--                                    Constr (
--                                       List (
--                                          LL [Exp (
--                                                 Constr (
--                                                    Lit (
--                                                       LAtom (
--                                                          Atom "unsigned"))))] (
--                                             Exp (
--                                                Constr (
--                                                   List (
--                                                      L [Exp (
--                                                            Constr (
--                                                               Lit (
--                                                                  LAtom (
--                                                                     Atom "big"))))])))))))],
--           BitString (
--             Exp (
--                Constr (
--                   Lit (
--                      LInt 3)))) [Exp (
--                                     Constr (
--                                        Lit (
--                                           LInt 8))),
--                                  Exp (
--                                    Constr (
--                                       Lit (
--                                          LInt 1))),
--                                  Exp (
--                                    Constr (
--                                       Lit (
--                                          LAtom (
--                                             Atom "integer")))),
--                                  Exp (
--                                    Constr (
--                                       List (
--                                          LL [Exp (
--                                                 Constr (
--                                                    Lit (
--                                                       LAtom (
--                                                          Atom "unsigned"))))] (
--                                             Exp (
--                                                Constr (
--                                                   List (
--                                                      L [Exp (
--                                                            Constr (
--                                                               Lit (
--                                                                  LAtom (
--                                                                     Atom "big"))))])))))))],
--           BitString (
--             Exp (
--                Constr (
--                   Var "Y"))) [Exp (
--                                  Constr (
--                                     Lit (
--                                        LInt 8))),
--                               Exp (
--                                 Constr (
--                                    Lit (
--                                       LInt 1))),
--                               Exp (
--                                 Constr (
--                                    Lit (
--                                       LAtom (
--                                          Atom "integer")))),
--                               Exp (
--                                 Constr (
--                                    List (
--                                       LL [Exp (
--                                              Constr (
--                                                 Lit (
--                                                    LAtom (
--                                                       Atom "unsigned"))))] (
--                                          Exp (
--                                             Constr (
--                                                List (
--                                                   L [Exp (
--                                                         Constr (
--                                                            Lit (
--                                                               LAtom (
--                                                                  Atom "big"))))])))))))]]

eval eCtx (Binary bs) = do
  bs' <- mapM (evalBitString eCtx) bs
  return $ ErlBinary $ runPut $ mapM_ putLazyByteString bs'

eval _ expr =
  errorL ["Unhandled expression: ", show expr]

evalBitString :: EvalCtx -> S.BitString Exps -> ErlProcess BS.ByteString
evalBitString eCtx (BitString e params) = do
  ErlNum v <- uevalExps eCtx e
  pars <- mapM (uevalExps eCtx) params
  let ((ErlNum 1):(ErlNum 8):_) = htrace ("Params: " ++ (show pars)) $ pars
  return $ runPut $ putWord8 $ fromInteger v

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
matchPat eCtx (PLit lit) term = do
  let lit' = literalToTerm lit
  if lit' == term
    then Just eCtx
    else Nothing
matchPat eCtx (PVar var) term = do
  return $ setupFunctionContext eCtx ([var], ErlSeq [term])

matchPat eCtx (PList (LL [h] t)) (ErlList (x:xs)) = do
  eCtx' <- matchPat eCtx h x
  matchPat eCtx' t (ErlList xs)
matchPat eCtx (PList _) (ErlList _) = Nothing

matchPat eCtx (PAlias (Alias var pat)) term = do
  eCtx' <- matchPat eCtx pat term
  return $ setupFunctionContext eCtx ([var], ErlSeq [term])

matchPat _eCtx pat term = do
  errorL ["matchPat: Not implemented matching of", show pat, "with", show term]

matchGuard :: EvalCtx -> S.Guard -> ErlProcess Bool
matchGuard eCtx (Guard exprs) = do
  evaled <- uevalExps eCtx exprs
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
  let frame = Frame { mfa = (modName erlmod, fn, arity),
                      args = Nothing,
                      pos = FilePos "" 0 }
  case erlmod of
    EModule _ emodule -> do
      -- evalModFn :: EvalCtx -> S.Module -> String -> ErlArity -> ErlProcess ErlTerm
      modify $ \ps ->
        ps { curr_mod = erlmod }
      local (\s -> (frame:s)) $ evalModFn emodule fn args
    HModule modname funs -> do
      let fun = M.lookup (fn, arity) funs `forceMaybeL` ["Function not found for:", showFunCall modname fn args]
      local (\s -> (frame:s)) $ applyFun fun args

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
              bargs = L.zipWith (\x y -> ([x], ErlSeq [y])) argNames args
              evalCtx' = setupFunctionContext' eCtx bargs
            in
             uevalExps evalCtx' expressions
          -- _ ->
          --   BifsCommon.bif_badarg_num
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
erlang_apply [lambda, ErlList args] = do
  case lambda of
    (ErlFunName name arity) -> do
      (EModule mn curMod) <- gets curr_mod
      let frame = Frame { mfa = (mn, name, arity),
                          args = Just $ [lambda, ErlList args],
                          pos = FilePos "" 0 }
      local (\s -> (frame:s)) $ evalModFn curMod name args
    fun@(ErlLambda name _argNames _eCtx _exprs) -> do
      let arity = (toInteger (length args))
      (EModule mn curMod) <- gets curr_mod
      let frame = Frame { mfa = (mn, name, arity),
                          args = Just $ [lambda, ErlList args],
                          pos = FilePos "" 0 }
      local (\s -> (frame:s)) $ applyFunLambda fun args
    _ -> do
      liftIO $ putStrLn "erlang:apply badarg type"
      BifsCommon.bif_badarg_t
erlang_apply _ = bif_badarg_num

erlang_spawn :: ErlStdFun
erlang_spawn [lambda] = do
  Left mmt <- gets mod_table
  cmod <- gets curr_mod
  pid <- lift $ lift $ spawnLocal $ localEvaluator cmod mmt ("erlang", "apply", [lambda, ErlList []])
  return $ ErlPid pid
erlang_spawn _ = bif_badarg_num

erlang_spawn_link :: ErlStdFun
erlang_spawn_link [lambda] = do
  p <- erlang_spawn [lambda]
  Erlang.erlang_link [p]
  return $ p
erlang_spawn_link _ = bif_badarg_num

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
    (("spawn_link", 1), ErlStdFun erlang_spawn_link),
    (("spawn", 1), ErlStdFun erlang_spawn)]

adjustErlangModule' :: ErlFunTable -> ErlFunTable
adjustErlangModule' funs =
  M.union evalBifs funs

-- TODO: remote evaluator can not accept MVar, so instead it should
-- ask boot/kernel/init process for MVar ModTable (does not work,
-- since local coordinator can not send me a message); so instead I'd
-- have to send a message to coordinator on remote node, which would
-- 'spawnLocal' actual process and supply it MVar ModTable
localEvaluator :: ErlModule -> MVar ModTable -> (ModName, FunName, [ErlTerm]) -> Process ()
localEvaluator cmod mmt (emod, fn, args) = do
  let runner = applyMFA emod fn args
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

bootProc :: Process ()
bootProc = do
  mmt <- liftIO $ newMVar newBaseModTable
  liftIO $ putStrLn "Boot process starting..."
  liftIO $ putStrLn "Running"
  pid <- spawnLocal (localEvaluator bootModule mmt ("init", "boot", [ErlList []]))
  mref <- monitor pid
  a <- expect :: Process ProcessMonitorNotification
  liftIO $ print $ show a
  return ()
