-- | Main entry point to the application.
module Main where

import qualified Data.Map as M
import Data.List as L
import Data.Char as C

import System.Random (getStdRandom, randomIO)

import Control.Monad.State
  (State, StateT, put, get, runState, evalState, evalStateT, runStateT,
   liftIO)
import Data.Map as M

import Language.CoreErlang.Parser as P
import Language.CoreErlang.Syntax as S
import Language.CoreErlang.Pretty as PP

import ErlHaskExamples

errorL :: [String] -> x
errorL args = error $ L.intercalate " " args

type Key = String

data ErlTerm = ErlList [ErlTerm] |
               ErlTuple [ErlTerm] |
               ErlAtom String |
               ErlNum Integer |
               ErlFloat Double
             deriving Show

type VarTable = M.Map String ErlTerm
type ProcessDictionary = M.Map String ErlTerm
type ModTable = M.Map String S.Module

data EvalCtx = ECtx VarTable
type ErlProcessState a = StateT (ModTable, ProcessDictionary) IO a

newEvalCtx :: EvalCtx
newEvalCtx = ECtx M.empty

newProcDict :: ProcessDictionary
newProcDict = M.empty

newModTable :: ModTable
newModTable = M.empty

newProcState :: (ModTable, ProcessDictionary)
newProcState = (newModTable, newProcDict)

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
eval eCtx (Lit l) = return $ literalToTerm l
eval eCtx (Tuple exps) = do
  elements <- mapM (evalExps eCtx) exps
  return $ ErlTuple elements
eval eCtx (Let (var,val) exps) = do
  value <- evalExps eCtx val
  let eCtx' = setupFunctionContext eCtx (var,value)
  evalExps eCtx' exps

eval eCtx (ModCall (mod0, arity0) args0) = do
  let evalExps' = evalExps eCtx
  mod <- evalExps' mod0
  arity <- evalExps' arity0
  args <- mapM evalExps' args0
  modCall mod arity args

eval (ECtx varTable) (Var var) = do
  let Just val = M.lookup var varTable
  return val

eval eCtx exp =
  error $ concat ["Unhandled expression: ", show exp]

modCall :: ErlTerm -> ErlTerm -> [ErlTerm] -> ErlProcessState ErlTerm
modCall (ErlAtom "erlang") (ErlAtom "display") (arg:[]) = do
  liftIO $ print arg
  return arg
modCall (ErlAtom "erlang") (ErlAtom "+") (a:b:[]) =
  return $ case (a, b) of
    (ErlNum aa, ErlNum bb) -> ErlNum (aa + bb)
    (ErlNum aa, ErlFloat bb) -> ErlFloat (fromInteger aa + bb)
    (ErlFloat aa, ErlNum bb) -> ErlFloat (aa + fromInteger bb)
    (ErlFloat aa, ErlFloat bb) -> ErlFloat (aa + bb)
modCall (ErlAtom "random") (ErlAtom "uniform") args = do
  value <- liftIO $ (randomIO :: IO Double)
  return $ ErlFloat value
modCall (ErlAtom mod) (ErlAtom fn) args = do
  (modTable, _) <- get
  case M.lookup mod modTable of
    -- Just ErlModule _ -> errorL ["Not yet done"]
    -- Just HaskellModule _ -> errorL ["Not yet done"]
    Just _ -> errorL ["Not yet done"]
    Nothing -> errorL ["modCall not yet implemented", mod, fn, show args]

modCall mod fn args =
  errorL ["Wrong type of call", show mod, show fn, show args]

setupFunctionContext :: EvalCtx -> ([Var], ErlTerm) -> EvalCtx
setupFunctionContext eCtx ([], _) = eCtx
setupFunctionContext (ECtx varTable) (x:xs, value) =
  let varTable' = M.insert x value varTable
  in setupFunctionContext (ECtx varTable') (xs, value)

findFn0 :: String -> Integer -> [FunDef] -> Maybe FunDef
findFn0 name arity funs =
  let
    aname = Atom name
    test = \(FunDef nm funs) ->
      let (Function ((Atom n), a)) = unann nm
      in (n == name) && (a == arity)
  in
   find test funs

findFn :: String -> Integer -> [FunDef] -> Maybe Exps
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
  error $ concat ["It is not a lambda", show e]

findModFn :: Module -> String -> Integer -> Maybe Exps
findModFn m name arity =
  let Module modName exports attributes funs = m
  in
   findFn name arity funs

evalModFn :: EvalCtx -> Module -> String -> Integer -> ErlProcessState ErlTerm
evalModFn eCtx mod fn arity = do
  let Just exp = findModFn mod fn arity
  evalExps eCtx exp


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
