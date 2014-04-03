-- | Main entry point to the application.
module Main where

-- import Debug.HTrace

import Data.Hashable

import qualified Data.Map as M
import qualified Data.List as L
import Data.Char as C

import Control.Monad.State (
  -- put,
  get,
  evalStateT)

import Control.Monad (foldM)

import Language.CoreErlang.Parser as P
import Language.CoreErlang.Syntax as S
import Language.CoreErlang.Pretty as PP

import ErlCore
import ErlBifs as Bifs

maybeError :: String -> Maybe a -> Either String a
maybeError what = maybe (Left what) Right

maybeErrorL :: [String] -> Maybe a -> Either String a
maybeErrorL what = maybe (Left (L.intercalate " " what)) Right

orFail :: Maybe a -> String -> Either String a
orFail = flip maybeError

orFailL :: Maybe a -> [String] -> Either String a
orFailL = flip maybeErrorL

errorL :: [String] -> x
errorL args = error $ L.intercalate " " args

showShortFunName :: String -> Arity -> String
showShortFunName fn arity =
  concat [fn, "/", show arity]

showFunName :: String -> String -> Arity -> String
showFunName emod fn arity =
  concat [emod, ":", fn, "/", show arity]

showFunCall :: String -> String -> [ErlTerm] -> String
showFunCall emod fn args =
  concat [emod, ":", fn, "(", L.intercalate "," (L.map show args), ")"]

unann :: Ann a -> a
unann (Constr a) = a
unann (Ann a _) = a

literalToTerm :: Literal -> ErlTerm
literalToTerm (LChar c) = ErlNum $ toInteger $ C.ord c
literalToTerm (LString str) = ErlList $ L.map (ErlNum . toInteger . C.ord) str
literalToTerm (LInt int) = ErlNum int
literalToTerm (LFloat double) = ErlFloat double
literalToTerm (LAtom (Atom atom_name)) = ErlAtom atom_name
literalToTerm LNil = ErlList []

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
-- eval = undefined
-- eval _ expr | htrace ("eval " ++ show expr) False = undefined
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
    Nothing -> errorL ["Missing binding?", var, show $ M.toList varTable]

eval eCtx (App lambda args) = do
  ((EModule curMod), _, _) <- get
  let evalExps' = evalExps eCtx
  lambda' <- evalExps' lambda
  args' <- mapM evalExps' args
  case lambda' of
    (ErlFunName name _arity) -> do
      evalModFn curMod name args'
    (ErlLambda _name _argNames fun) -> do
      fun args'
    _ -> errorL ["Can not apply", show lambda']

eval _ (Fun (Function ((Atom name), arity))) =
  return $ ErlFunName name arity

eval eCtx (Lambda argNames exprs) = do
  return $ ErlLambda (show $ hash exprs) argNames (expsToErlFun eCtx argNames exprs)

eval eCtx (Case val alts) = do
  let alts' = map unann alts
  val' <- evalExps eCtx val
  matchAlts eCtx val' alts'

eval eCtx (List list) = do
  exprListToTerm eCtx list

eval eCtx (Op (Atom op) exprs) = do
  args' <- mapM evalExps' args
  

eval _ expr =
  error $ concat ["Unhandled expression: ", show expr]

matchAlts :: EvalCtx -> ErlTerm -> [S.Alt] -> ErlProcessState ErlTerm
matchAlts _ _ [] = errorL ["No matching clauses"]
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

matchPat _ pat term = errorL ["Not implemented matching of", show pat, show term]

matchGuard :: EvalCtx -> S.Guard -> ErlProcessState Bool
matchGuard eCtx (Guard exprs) = do
  evaled <- evalExps eCtx exprs
  case evaled of
    ErlAtom "true" ->
      return True
    _ -> return False

modCall :: ErlTerm -> ErlTerm -> [ErlTerm] -> ErlProcessState ErlTerm
modCall (ErlAtom emod) (ErlAtom fn) args = do
  (_, modTable, _) <- get
  let arity = (toInteger (length args))
  let call = do
        erlmod <- M.lookup emod modTable `orFailL` ["Module not found for:", showFunCall emod fn args]
        case erlmod of
          EModule emodule -> do
            -- evalModFn :: EvalCtx -> S.Module -> String -> Arity -> ErlProcessState ErlTerm
            return $ evalModFn emodule fn args
          HModule funs -> do
            fun <- M.lookup (fn, arity) funs `orFailL` ["Function not found for:", showFunCall emod fn args]
            return $ applyFunLambda fun args

  either error id call

modCall emod fn args =
  errorL ["Wrong type of call", show emod, show fn, show args]

setupFunctionContext :: EvalCtx -> ([Var], ErlTerm) -> EvalCtx
setupFunctionContext eCtx ([], _) = eCtx
setupFunctionContext (ECtx varTable) (x:xs, value) =
  let varTable' = M.insert x value varTable
  in setupFunctionContext (ECtx varTable') (xs, value)

setupFunctionContext' :: EvalCtx -> [([Var], ErlTerm)] -> EvalCtx
setupFunctionContext' eCtx args =
  L.foldl (\oCtx arg -> setupFunctionContext oCtx arg) eCtx args

unlambda :: S.Exp -> S.Exps
unlambda (Lambda _ exps) = exps
unlambda e =
  errorL ["It is not a lambda", show e]

applyFunLambda :: ErlFun -> [ErlTerm] -> ErlProcessState ErlTerm
applyFunLambda fun args = fun args

-- applyELambda :: EvalCtx -> S.Exps -> [Var] -> [ErlTerm] -> ErlProcessState ErlTerm
-- applyELambda eCtx expressions names args =
--   let fun = expsToErlFun eCtx names expressions
--   in applyFunLambda fun args 

expsToErlFun :: EvalCtx -> [Var] -> S.Exps -> ErlFun
expsToErlFun eCtx argNames expressions =
  let arity = length argNames
  in
   \args -> do     
     case length args of
       a | a == arity ->
         let
           bargs = L.zipWith (\x y -> ([x],y)) argNames args
           evalCtx' = setupFunctionContext' eCtx bargs
         in
          evalExps evalCtx' expressions
       _ -> errorL ["Wrong arity!", show argNames, show args, show expressions]       

findExportedFunction :: String -> Arity -> [FunDef] -> Maybe FunDef
findExportedFunction name arity funs =
  let
    test = \(FunDef nm _) ->
      let (Function ((Atom n), a)) = unann nm
      in (n == name) && (a == arity)
  in
   L.find test funs

findFn :: String -> Arity -> [FunDef] -> Maybe ErlFun
findFn name arity funs = do
  (FunDef _ aexp) <- findExportedFunction name arity funs
  let (Lambda vars exprs) = unann aexp
  return $ expsToErlFun newEvalCtx vars exprs

findModFn :: S.Module -> String -> Arity -> Maybe ErlFun
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
      errorL ["Can not find", showShortFunName fn arity, "in", show emod]


newEvalCtx :: EvalCtx
newEvalCtx = ECtx M.empty

newProcDict :: ProcessDictionary
newProcDict = M.empty

newModTable :: ModTable
newModTable =
  let bifs = Bifs.all
  in M.fromList bifs

newProcState :: ErlModule -> (ErlModule, ModTable, ProcessDictionary)
newProcState emod = (emod, newModTable, newProcDict)

loadEModule :: String -> IO (Either String S.Module)
loadEModule moduleName = do
  fileContent <- readFile ("samples/" ++ moduleName ++ ".core") -- `catch` \_ -> do return $ Left ""
  case P.parseModule fileContent of
    Left er ->  do
      return $ Left $ show er
    Right m -> do
      return $ Right (unann m)

-- | The main entry point.
main :: IO ()
main = do
  Right boot <- loadEModule "boot"
  let boot' = EModule boot
  putStrLn $ PP.prettyPrint boot
  let modTable0 = newModTable
  let modTable = M.insert "boot" boot' modTable0

  let runner = evalModFn boot "start" []
  result <- evalStateT runner (boot', modTable, newProcDict) 
  print result
  return ()




-- eval fctx (Put key value) = do
--   ctx <- get
--   evaluated <- eval fctx value
--   let procDict = M.insert key evaluated (pcProcDict ctx)
--   put $ ctx {pcProcDict = procDict}
--   return evaluated

-- eval _ (Get key) = do
--   ctx <- get
--   case M.lookup key (pcProcDict ctx) of
--     Nothing -> return $ ErlangTerm ["Fuck", "you"]
--     Just value -> return value

-- eval _ (Literal term) = return term

-- eval (ECtx eCtx) (Variable name) =
--   case M.lookup name eCtx of
--     Nothing -> error "nope dope"
--     Just value -> return value
