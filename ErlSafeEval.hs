{-# LANGUAGE TemplateHaskell, DataKinds, DeriveGeneric, StandaloneDeriving, DeriveDataTypeable, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ErlSafeEval where

import Debug.HTrace

import Data.Hashable

import qualified Data.Map as M
import qualified Data.List as L
import Data.Char as C

import Control.Monad (foldM)

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

evalExps :: EvalCtx -> S.Exps -> ErlTerm
evalExps eCtx (Exp e) = eval eCtx (unann e)
evalExps eCtx (Exps aexs) = do
  let exs = unann aexs
      xs = L.map unann exs
  last $ L.map (eval eCtx) xs

eval :: EvalCtx -> S.Exp -> ErlTerm
eval _ expr | htrace ("eval " ++ show expr) False = undefined
eval eCtx (Seq a b) =
  let _ = evalExps eCtx a
  in evalExps eCtx b

eval _ (Lit l) = literalToTerm l
eval eCtx (Tuple exps) =
  let elements = map (evalExps eCtx) exps
  in ErlTuple elements
eval eCtx (Let (var,val) exps) =
  let value = evalExps eCtx val
      eCtx' = setupFunctionContext eCtx (var,value)
  in evalExps eCtx' exps

-- eval eCtx (ModCall (mod0, arity0) args0) =
--   let evalExps' = evalExps eCtx
--   emod <- evalExps' mod0
--   arity <- evalExps' arity0
--   args <- mapM evalExps' args0
--   modCall emod arity args

-- eval (ECtx varTable) (Var var) =
--   case M.lookup var varTable of
--     Just val -> return val
--     Nothing -> errorL ["Missing binding?", var, show $ M.toList varTable]

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

matchGuard :: EvalCtx -> S.Guard -> Bool
matchGuard eCtx (Guard exprs) =
  let evaled = evalExps eCtx exprs
  in
   case evaled of
     ErlAtom "true" ->
       True
     _ ->
       False


-- modCall :: ErlTerm -> ErlTerm -> [ErlTerm] -> ErlTerm
-- modCall (ErlAtom emod) (ErlAtom fn) args =
--   applyMFA emod fn args

-- applyMFA :: ModName -> FunName -> [ErlTerm] -> ErlTerm
-- applyMFA modname fn args = do
--   applyFunInMod modname fn args

-- applyFunInMod :: ErlModule -> FunName -> [ErlTerm] -> ErlTerm
-- applyFunInMod erlmod fn args =
--   let arity = (toInteger (length args))
--   in case erlmod of
--     EModule _ emodule -> do
--       evalModFn emodule fn args
--     HModule modname funs -> do
--       let fun = M.lookup (fn, arity) funs `forceMaybeL` ["Function not found for:", showFunCall modname fn args]
--       applyHLambda fun args

-- evalModFn :: S.Module -> String -> [ErlTerm] -> ErlTerm
-- evalModFn emod fn args = do
--   let arity = (toInteger (length args))
--     in case findModFn emod fn arity of
--     Just fun ->
--       applyFunLambda fun args
--     Nothing ->
--       dieL ["Can not find", showShortFunName fn arity, "in", show emod]
