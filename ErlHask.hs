-- | Main entry point to the application.
module Main where

import qualified Data.Map as M
import Data.List

import Control.Monad.State (State, put, get, runState)
import Data.Map as M

import Language.CoreErlang.Parser as P
import Language.CoreErlang.Syntax as S
import Language.CoreErlang.Pretty as PP

type Key = String

simpleModule = unlines [
    "module 'simple' ['main'/0]",
    "    attributes []",
    "'main'/0 =",
    "    fun () ->",
    "       let <X> =",
    "           call 'mod':'read'",
    "               ()",
    "       in",
    "           call 'erlang':'display'",
    "               (X)",
    "end"]

data ErlTerm = ErlList [ErlTerm] |
               ErlAtom String |
               ErlFun String Int |
               ErlNum Integer

type VarTable = M.Map String ErlTerm
newtype ProcessDictionary = VarTable
type ModTable = M.Map String (S.Ann S.Module)

data EvalCtx = ECtx VarTable
type ErlProcessState a = State ProcessDictionary a

eval :: EvalCtx -> S.Exp -> ErlProcessState ErlTerm
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

-- eval (ECtx fCtx) (Variable name) =
--   case M.lookup name fCtx of
--     Nothing -> error "nope dope"
--     Just value -> return value

eval fCtx (Let bindings exprs) = do
  fCtx' <- setupFunctionContext fCtx bindings
  fmap last $ mapM (eval fCtx') exprs

setupFunctionContext :: EvalCtx -> [(Key, ErlTerm)] -> EvalCtx
setupFunctionContext _ _ = undefined


findFn :: String -> Integer -> [FunDef] -> Maybe FunDef
findFn name arity funs =
  let test = \FunDef fd -> case fd of
        (Ann (Function (Atom "main" 0))) -> True
        _ -> False
  in find test funs

-- | The main entry point.
main :: IO ()
main = do
    putStrLn "Welcome to FP Haskell Center!"
    putStrLn "Have a good day!"
    case P.parseModule simpleModule of
        Left er ->  do
            putStrLn "Error"
            print er
        Right m@(Ann Module name _ funs) -> do
            putStrLn $ PP.prettyPrint m
            FunDef _ (Ann exp) <- findFn "main" 0
            eval exp
