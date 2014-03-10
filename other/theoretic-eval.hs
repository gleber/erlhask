import Control.Monad.State (State, put, get, runState)
import Data.Map as M

type Key = String

data Exp
  = Put Key Exp
  | Get Key
  | Let [(Key, Exp)] [Exp]
  | Literal ErlangTerm
  | Variable String

newtype ErlangTerm = ErlangTerm [String]

type ProcDict = M.Map String ErlangTerm
type VarTable = M.Map String ErlangTerm

data FuncCtx = FCtx VarTable

data ProcessContext = PCtx {
    pcProcDict :: ProcDict  -- globally mutable
  }

-- eval :: Exp -> ProcessContext -> (ProcessContext, ErlangTerm)
-- eval foo = undefined

type MyState a = State ProcessContext a

evalFooExpr :: Exp -> MyState Int
evalFooExpr = undefined

eval :: FuncCtx -> Exp -> MyState ErlangTerm
eval fctx (Put key value) = do
  ctx <- get
  evaluated <- eval fctx value
  let procDict = M.insert key evaluated (pcProcDict ctx)
  put $ ctx {pcProcDict = procDict}
  return evaluated

eval _ (Get key) = do
  ctx <- get
  case M.lookup key (pcProcDict ctx) of
    Nothing -> return $ ErlangTerm ["Fuck", "you"]
    Just value -> return value

eval _ (Literal term) = return term

eval (FCtx fCtx) (Variable name) =
  case M.lookup name fCtx of
    Nothing -> error "nope dope"
    Just value -> return value

eval fCtx (Let bindings exprs) = do
  fCtx' <- setupFunctionContext fCtx bindings
--  results <- mapM (eval fCtx') exprs
--  return $ tail results
  fmap last $ mapM (eval fCtx') exprs

setupFunctionContext :: FuncCtx -> [(Key, Exp)] -> MyState FuncCtx
setupFunctionContext _ _ = undefined

main :: IO ()
main = do
  return ()
