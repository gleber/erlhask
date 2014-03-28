-- | Main entry point to the application.
module Main where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Char as C

import Control.Monad.State (put, get, evalStateT, liftIO)

import Language.CoreErlang.Parser as P
import Language.CoreErlang.Syntax as S
import Language.CoreErlang.Pretty as PP

import ErlHaskExamples
import ErlCore
import ErlBifs as Bifs

maybeError :: String -> Maybe a -> Either String a
maybeError what = maybe (Left what) Right

maybeErrorL :: [String] -> Maybe a -> Either String a
maybeErrorL what = maybe (Left (L.intercalate " " what)) Right

orFail = flip maybeError

orFailL = flip maybeErrorL

errorL :: [String] -> x
errorL args = error $ L.intercalate " " args

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

eval _ expr =
  error $ concat ["Unhandled expression: ", show expr]


modCall :: ErlTerm -> ErlTerm -> [ErlTerm] -> ErlProcessState ErlTerm
modCall (ErlAtom emod) (ErlAtom fn) args = do
  (modTable, _) <- get
  let arity = (toInteger (length args))
  let call = do
        erlmod <- M.lookup emod modTable `orFailL` ["Module not found for:", showFunCall emod fn args]
        case erlmod of
          EModule _ -> error "Loading of EModules is not yet done"
          HModule funs -> do
            fun <- M.lookup (fn, arity) funs `orFailL` ["Function not found for:", showFunCall emod fn args]
            return $ fun args

  either error id call

modCall emod fn args =
  errorL ["Wrong type of call", show emod, show fn, show args]

setupFunctionContext :: EvalCtx -> ([Var], ErlTerm) -> EvalCtx
setupFunctionContext eCtx ([], _) = eCtx
setupFunctionContext (ECtx varTable) (x:xs, value) =
  let varTable' = M.insert x value varTable
  in setupFunctionContext (ECtx varTable') (xs, value)

findFn0 :: String -> Arity -> [FunDef] -> Maybe FunDef
findFn0 name arity funs =
  let
    test = \(FunDef nm _) ->
      let (Function ((Atom n), a)) = unann nm
      in (n == name) && (a == arity)
  in
   L.find test funs

findFn :: String -> Arity -> [FunDef] -> Maybe Exps
findFn name arity funs =
  case findFn0 name arity funs of
    Just (FunDef _ aexp) ->
      let lambda = unann aexp
      in Just $ unlambda lambda
    _ ->
      Nothing

unlambda :: S.Exp -> S.Exps
unlambda (Lambda [] exps) = exps
unlambda e =
  errorL ["It is not a lambda", show e]

findModFn :: S.Module -> String -> Arity -> Maybe Exps
findModFn m name arity =
  let Module _modName _exports _attributes funs = m
  in
   findFn name arity funs

evalModFn :: EvalCtx -> S.Module -> String -> Arity -> ErlProcessState ErlTerm
evalModFn eCtx emod fn arity = do
  let Just expr = findModFn emod fn arity
  evalExps eCtx expr


newEvalCtx :: EvalCtx
newEvalCtx = ECtx M.empty

newProcDict :: ProcessDictionary
newProcDict = M.empty

newModTable :: ModTable
newModTable =
  M.fromList Bifs.all

newProcState :: (ModTable, ProcessDictionary)
newProcState = (newModTable, newProcDict)

-- loadEModule :: String -> ErlProcessState ()
-- loadEModule = do

-- | The main entry point.
main :: IO ()
main = do
  putStrLn "Have a good day!"
  case P.parseModule twoLetMod of
    Left er ->  do
      putStrLn "Error"
      print er
    Right m -> do
      putStrLn $ PP.prettyPrint m
      let eCtx = newEvalCtx
      let runner = evalModFn eCtx (unann m) "main" 0
      result <- evalStateT runner newProcState
      return ()
      -- print result




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
