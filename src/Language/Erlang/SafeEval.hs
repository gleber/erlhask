{-# LANGUAGE TemplateHaskell, DataKinds, DeriveGeneric, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}

module ErlSafeEval where

import Debug.HTrace

import Data.Hashable

import qualified Data.Map as M
import qualified Data.List as L
import Data.Char as C

import Control.Monad (foldM)
import Control.Monad.RWS (gets)

import Language.CoreErlang.Parser as P
import Language.CoreErlang.Syntax as S
import Language.CoreErlang.Pretty as PP

import ErlCore
import ErlUtil
import ErlModules
import ErlLangCore

-- exprListToTerm :: EvalCtx ->  S.List S.Exps -> ErlTerm
-- exprListToTerm eCtx (LL exprs tail) = do
--   vals <- mapM (evalExps eCtx) exprs
--   (ErlList tail) <- evalExps eCtx tail
--   return $ ErlList (vals ++ tail)
-- exprListToTerm eCtx (L exprs) = do
--   vals <- mapM (evalExps eCtx) exprs
--   return $ ErlList vals

-- -- PList (LL [PVar "K"] (PList (LL [PVar "L"] (PVar "_cor8")))) [1, 2, 3]

evalExps :: EvalCtx -> S.Exps -> ErlPure ErlTerm
evalExps eCtx (Exp e) = eval eCtx (unann e)
evalExps eCtx (Exps aexs) = do
  let exs = unann aexs
      xs = L.map unann exs
  last $ L.map (eval eCtx) xs

eval :: EvalCtx -> S.Exp -> ErlPure ErlTerm
eval _ expr | htrace ("eval " ++ show expr) False = undefined
eval eCtx (Seq a b) =
  let _ = evalExps eCtx a
  in evalExps eCtx b

eval _ (Lit l) = return $ literalToTerm l
eval eCtx (Tuple exps) = do
  elements <- mapM (evalExps eCtx) exps
  return $ ErlTuple elements
eval eCtx (Let (var,val) exps) = do
  value <- evalExps eCtx val
  let eCtx' = setupFunctionContext eCtx (var, value)
  evalExps eCtx' exps

{-

                                try
                                    let <_cor3> =
                                        call 'erlang':'float'
                                            (X)
                                    in  call 'erlang':'>'
                                            (_cor3, 1)
                                of <Try> ->
                                    Try
                                catch <T,R> ->
                                    'false'

  (Try
   (Exp
    (Constr
     (Let
      (["_cor3"],Exp
                 (Constr
                  (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))),
                            Exp (Constr (Lit (LAtom (Atom "float")))))
                   [Exp (Constr (Var "X"))])
                 ))
      (Exp
       (Constr
        (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))),
                  Exp (Constr (Lit (LAtom (Atom ">")))))
         [Exp (Constr (Var "_cor3")),
          Exp (Constr (Lit (LInt 1)))]))))))

   (["Try"],Exp
            (Constr
             (Let
              (["_cor3"],Exp
                         (Constr
                          (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))),
                                    Exp (Constr (Lit (LAtom (Atom "float")))))
                           [Exp (Constr (Var "X"))])))
              (Exp
               (Constr
                (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))),
                          Exp (Constr (Lit (LAtom (Atom ">")))))
                 [Exp (Constr (Var "_cor3")),
                  Exp (Constr (Lit (LInt 1)))]))))))
   (["T","R"],Exp
              (Constr
               (Var "Try"))))
-}

eval eCtx (Try body (bodyBind, success) (catchVars, catchBody)) = do
  mt <- gets mod_table
  let bodyRes = runErlPure mt $ evalExps eCtx body
  case bodyRes of
    Right val -> do
      let eCtx' = setupFunctionContext eCtx (bodyBind, val)
      evalExps eCtx' success
    Left (ErlException {}) -> do
      -- need to setupFunctionContext here somehow, but not sure what are catchVars
      evalExps eCtx catchBody

eval eCtx (ModCall (mod0, arity0) args0) = do
  let evalExps' = evalExps eCtx
  emod <- evalExps' mod0
  arity <- evalExps' arity0
  args <- mapM evalExps' args0
  modCall emod arity args

eval (ECtx varTable) (Var var) =
  case M.lookup var varTable of
    Just val -> return val
    Nothing -> errorL ["Missing binding?", var, show $ M.toList varTable]

-- -- eval eCtx (App lambda args) =
-- --   (mod, _, _) <- get
-- --   case mod of
-- --     EModule _ curMod -> do
-- --       let evalExps' = evalExps eCtx
-- --       lambda' <- evalExps' lambda
-- --       args' <- mapM evalExps' args
-- --       case lambda' of
-- --         (ErlFunName name _arity) -> do
-- --           evalModFn curMod name args'
-- --         fun@(ErlLambda _name _argNames _eCtx _exprs) -> do
-- --           applyFunLambda fun args'
-- --         _ ->
-- --           errorL ["Can not apply", show lambda']
-- --     _ ->
-- --       errorL ["Unable to apply things in ", show mod]

-- eval _ (Fun (Function ((Atom name), arity))) =
--   ErlFunName name arity

-- eval eCtx (Lambda argNames exprs) =
--   ErlLambda (show $ hash exprs) argNames eCtx exprs

-- eval eCtx (Case val alts) =
--   let alts' = map unann alts
--   val' <- evalExps eCtx val
--   matchAlts eCtx val' alts'

-- eval eCtx (List list) =
--   exprListToTerm eCtx list

-- eval eCtx (Op (Atom op) args) =
--   let evalExps' = evalExps eCtx
--   args' <- mapM evalExps' args
--   case op of
--     "match_fail" -> errorL $ map show args'


-- eval _ expr =
--   errorL ["Unhandled expression: ", show expr]

-- matchAlts :: EvalCtx -> ErlTerm -> [S.Alt] -> ErlTerm
-- matchAlts _ _ [] = dieL ["No matching clauses"]
-- matchAlts eCtx0 val (alt:xs) = do
--   let (Alt pats guard exprs) = alt
--   let matched = matchPats eCtx0 pats val
--   case matched of
--     Just eCtx -> do
--       guarded <- matchGuard eCtx guard
--       case guarded of
--         True ->
--           evalExps eCtx exprs
--         False ->
--           matchAlts eCtx val xs
--     Nothing ->
--       matchAlts eCtx0 val xs

-- -- (Pats [PTuple [PLit (LAtom (Atom "ok")),
-- --                PVar "Z"]])
-- matchPats :: EvalCtx -> S.Pats -> ErlTerm -> Maybe EvalCtx
-- matchPats eCtx (Pats [pat]) term = matchPats eCtx (Pat pat) term
-- matchPats eCtx (Pat pat) term = do
--   matchPat eCtx pat term

-- matchPat :: EvalCtx -> S.Pat -> ErlTerm -> Maybe EvalCtx
-- matchPat eCtx (PTuple pat) (ErlTuple term) = do
--   if
--     L.length pat == L.length term
--     then foldM (\ctx (p,e) -> matchPat ctx p e) eCtx (L.zip pat term)
--     else Nothing
-- matchPat eCtx (PLit lit) term = do
--   let lit' = literalToTerm lit
--   if lit' == term
--     then Just eCtx
--     else Nothing
-- matchPat eCtx (PVar var) term = do
--   return $ setupFunctionContext eCtx ([var], term)

-- -- PList (LL [PVar "K"] (PList (LL [PVar "L"] (PVar "_cor8")))) [1, 2, 3]
-- matchPat eCtx (PList (LL [head] tail)) (ErlList (x:xs)) = do
--   eCtx' <- matchPat eCtx head x
--   matchPat eCtx' tail (ErlList xs)

-- -- matchPat _ pat term = errorL ["Not implemented matching of", show pat, show term]

matchGuard :: EvalCtx -> ModTable -> S.Guard -> Bool
matchGuard eCtx mt (Guard exprs) =
  case runErlPure mt $ evalExps eCtx exprs of
    Right (ErlAtom "true") ->
      True
    _ ->
      False


modCall :: ErlTerm -> ErlTerm -> [ErlTerm] -> ErlPure ErlTerm
modCall (ErlAtom emod) (ErlAtom fn) args =
  applyMFA emod fn args

applyMFA :: ModName -> FunName -> [ErlTerm] -> ErlPure ErlTerm
applyMFA modname fn args = do
  erlmod <- getModule modname
  applyFunInMod erlmod fn args

applyFunInMod :: ErlModule -> FunName -> [ErlTerm] -> ErlPure ErlTerm
applyFunInMod erlmod fn args = do
  let arity = toInteger (length args)
  case erlmod of
    EModule _ emodule -> do
      errorL ["Should call only BIFs in safe eval mode"]
    HModule modname funs -> do
      let fun = M.lookup (fn, arity) funs `forceMaybeL` ["Function not found for:", showFunCall modname fn args]
      applyFun fun args

applyFun :: ErlFun -> [ErlTerm] -> ErlPure ErlTerm
applyFun (ErlStdFun fun) args = errorL ["Can't run non-pure function in Safe mode."]
applyFun (ErlPureFun fun) args = applyPureFun fun args

applyPureFun :: ErlPureFun -> [ErlTerm] -> ErlGeneric ErlTerm
applyPureFun fun args = fun args
