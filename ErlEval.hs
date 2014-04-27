{-# LANGUAGE TemplateHaskell, DataKinds, DeriveGeneric, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ErlEval where


import Prelude hiding (catch)

import System.IO.Unsafe (unsafePerformIO)

import Debug.HTrace

import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Control.Distributed.Process.Internal.Types (
  runLocalProcess,
  lpidCounter, lpidUnique, LocalProcessId(..))
import Network.Transport.TCP

import Data.Hashable
import Data.Binary (Binary)
import Data.Typeable
import GHC.Generics
import Data.Either.Utils

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Applicative ((<$>))
import Control.Exception (Exception(..), throw, throwIO, SomeException)

import Control.Concurrent
import Control.Concurrent.MVar


import qualified Data.Map as M
import qualified Data.List as L
import Data.Char as C

import Control.Monad.State (
  put,
  get,
  modify,
  evalStateT)

import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)

-- import Control.Exception.Lifted

import Language.CoreErlang.Parser as P
import Language.CoreErlang.Syntax as S
import Language.CoreErlang.Pretty as PP

import ErlCore
import ErlUtil
import ErlModules
import ErlLangCore
import ErlBifs as Bifs
import ErlBifsCommon as BifsCommon
import ErlSafeEval as Safe

-- (LL [Exp (Constr (Lit (LInt 1)))]
--  (Exp (Constr
--        (List (LL [Exp (Constr (Lit (LInt 2)))]
--               (Exp (Constr
--                     (List (L [Exp (Constr (Lit (LInt 3)))]
--                           )))))))))
exprListToTerm :: EvalCtx ->  S.List S.Exps -> ErlProcessState ErlTerm
exprListToTerm eCtx (LL exprs tail) = do
  vals <- mapM (evalExps eCtx) exprs
  (ErlList tail) <- evalExps eCtx tail
  return $ ErlList (vals ++ tail)
exprListToTerm eCtx (L exprs) = do
  vals <- mapM (evalExps eCtx) exprs
  return $ ErlList vals

-- PList (LL [PVar "K"] (PList (LL [PVar "L"] (PVar "_cor8")))) [1, 2, 3]

evalExps :: EvalCtx -> S.Exps -> ErlProcessState ErlTerm
evalExps eCtx (Exp e) = eval eCtx (unann e)
evalExps eCtx (Exps aexs) = do
  let exs = unann aexs
      xs = L.map unann exs
  fmap last $ mapM (eval eCtx) xs

eval :: EvalCtx -> S.Exp -> ErlProcessState ErlTerm
--eval _ expr | htrace ("eval " ++ show expr) False = undefined
eval eCtx (Seq a b) = do
  evalExps eCtx a
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
  (mod, _, _) <- get
  case mod of
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
      errorL ["Unable to apply things in ", show mod]

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

eval eCtx (Rec alts (TimeOut time timeoutExps)) = do
  time' <- evalExps eCtx time
  case time' of
    ErlNum time'' -> do
      let alts' = map unann alts
      matches <- receiveMatches eCtx alts'
      Just res <- lift $ receiveTimeout (fromInteger time'') matches
      return res
    _ ->
      BifsCommon.bif_badarg_t



eval _ expr =
  errorL ["Unhandled expression: ", show expr]



receiveMatches :: EvalCtx -> [S.Alt] -> ErlProcessState [Match ErlTerm]
receiveMatches eCtx0 alts = do
  pid <- lift $ getSelfPid
  return $ map (\(Alt pats guard exprs) ->
    (matchIf (\(msg :: ErlTerm) ->

               let matched = matchPats eCtx0 pats msg
               in
                case matched of
                  Just eCtx -> do
                    -- safeEval pid $ matchGuard eCtx guard
                    True
                  Nothing ->
                    False
             )
             (\(msg :: ErlTerm) ->
                 -- let Just eCtx = matchPats eCtx0 pats msg
                 -- evalExps eCtx exprs
                 return $ msg
             )) :: Match ErlTerm) alts

matchAlts :: EvalCtx -> ErlTerm -> [S.Alt] -> ErlProcessState ErlTerm
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

-- PList (LL [PVar "K"] (PList (LL [PVar "L"] (PVar "_cor8")))) [1, 2, 3]
matchPat eCtx (PList (LL [head] tail)) (ErlList (x:xs)) = do
  eCtx' <- matchPat eCtx head x
  matchPat eCtx' tail (ErlList xs)

-- matchPat _ pat term = errorL ["Not implemented matching of", show pat, show term]

matchGuard :: EvalCtx -> S.Guard -> ErlProcessState Bool
matchGuard eCtx (Guard exprs) = do
  evaled <- evalExps eCtx exprs
  case evaled of
    ErlAtom "true" ->
      return True
    _ -> return False

modCall :: ErlTerm -> ErlTerm -> [ErlTerm] -> ErlProcessState ErlTerm
modCall (ErlAtom emod) (ErlAtom fn) args =
  applyMFA emod fn args

modCall emod fn args =
  dieL ["Wrong type of call", show emod, show fn, show args]

applyMFA :: ModName -> FunName -> [ErlTerm] -> ErlProcessState ErlTerm
applyMFA modname fn args = do
  erlmod <- ensureEModule modname
  (_, modTable, _) <- get
  applyFunInMod erlmod fn args

applyFunInMod :: ErlModule -> FunName -> [ErlTerm] -> ErlProcessState ErlTerm
applyFunInMod erlmod fn args = do
  let arity = (toInteger (length args))
  case erlmod of
    EModule _ emodule -> do
      -- evalModFn :: EvalCtx -> S.Module -> String -> ErlArity -> ErlProcessState ErlTerm
      modify $ \(_, mt, c) ->
        (erlmod, mt, c)
      evalModFn emodule fn args
    HModule modname funs -> do
      let fun = M.lookup (fn, arity) funs `forceMaybeL` ["Function not found for:", showFunCall modname fn args]
      applyFun fun args

applyFunLambda :: ErlTerm -> [ErlTerm] -> ErlProcessState ErlTerm
applyFunLambda (ErlLambda _name names ctx exprs) args =
  applyELambda ctx exprs names args

applyELambda :: EvalCtx -> S.Exps -> [Var] -> [ErlTerm] -> ErlProcessState ErlTerm
applyELambda eCtx expressions names args =
  let fun = expsToErlFun eCtx names expressions
  in applyFun fun args

applyFun :: ErlFun -> [ErlTerm] -> ErlProcessState ErlTerm
applyFun (ErlStdFun fun) args = applyStdFun fun args
applyFun (ErlPureFun fun) args = return $ applyPureFun fun args

applyStdFun :: ErlStdFun -> [ErlTerm] -> ErlProcessState ErlTerm
applyStdFun fun args = fun args

applyPureFun :: ErlPureFun -> [ErlTerm] -> ErlTerm
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

evalModFn :: S.Module -> String -> [ErlTerm] -> ErlProcessState ErlTerm
evalModFn emod fn args = do
  let arity = (toInteger (length args))
    in case findModFn emod fn arity of
    Just fun ->
      applyFunLambda fun args
    Nothing ->
      dieL ["Can not find", showShortFunName fn arity, "in", show emod]

erlang_apply :: [ErlTerm] -> ErlProcessState ErlTerm
erlang_apply (lambda:(ErlList args):[]) = do
  case lambda of
    (ErlFunName name _arity) -> do
      ((EModule _ curMod), _, _) <- get
      evalModFn curMod name args
    fun@(ErlLambda _name _argNames _eCtx _exprs) -> do
      applyFunLambda fun args
    _ ->
      BifsCommon.bif_badarg_t
erlang_apply _ = bif_badarg_num

erlang_spawn :: [ErlTerm] -> ErlProcessState ErlTerm
erlang_spawn (lambda:[]) = do
  pid <- lift $ spawnLocal (evaluator ("erlang", "apply", [lambda, ErlList []]))
  return $ ErlPid pid
erlang_spawn _ = bif_badarg_num

newBaseModTable :: ModTable
newBaseModTable =
  M.adjust adjustErlangModule "erlang" Bifs.newBifsModTable

adjustErlangModule :: ErlModule -> ErlModule
adjustErlangModule (HModule "erlang" funs) =
  HModule "erlang" $ adjustErlangModule' funs

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
  -- liftIO $ putStrLn "Evaluating"
  let runner = applyMFA emod fn args
  let eval = do
        result <- evalStateT runner (bootModule, newBaseModTable, newProcDict)
        -- liftIO $ putStrLn "Done"
        -- liftIO $ print result
        return ()
  catch eval (\(e :: SomeException) -> do
                 liftIO $ print (show e)
                 throw e)
remotable ['evaluator]