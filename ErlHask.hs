-- | Main entry point to the application.
module Main where

-- import Debug.HTrace

import qualified Data.Map as M
import qualified Data.List as L
import Data.Char as C

import Control.Monad.State (
  -- put,
  get,
  evalStateT)

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
  let Just val = M.lookup var varTable
  return val

eval eCtx (App lambda args) = do
  ((EModule curMod), _, _) <- get
  let evalExps' = evalExps eCtx
  lambda' <- evalExps' lambda
  args' <- mapM evalExps' args
  case lambda' of
    (ErlFunName name _arity) -> do
      evalModFn curMod name args'
    (ErlLambda _argNames fun) -> do
      fun args'
    _ -> errorL ["Can not apply", show lambda']

eval _ (Fun (Function ((Atom name), arity))) =
  return $ ErlFunName name arity

eval eCtx (Lambda argNames exprs) = do
  return $ ErlLambda argNames $ expsToErlFun eCtx argNames exprs

eval _ expr =
  error $ concat ["Unhandled expression: ", show expr]


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
