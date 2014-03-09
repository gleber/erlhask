-- | Main entry point to the application.
module Main where

import qualified Data.Map as M
import Data.List as L
import Data.Char as C

import Control.Monad.State (State, put, get, runState, evalState)
import Data.Map as M

import Language.CoreErlang.Parser as P
import Language.CoreErlang.Syntax as S
import Language.CoreErlang.Pretty as PP

type Key = String

twoLetMod = unlines [
    "module 'simple' ['main'/0]",
    "    attributes []",
    "'main'/0 = ",
    "%% Line 5",
    "fun () ->",
    "    let <X> =",
    "        %% Line 6",
    "        call 'random':'uniform'",
    "            ()",
    "    in  let <Y> =",
    "            %% Line 7",
    "            call 'erlang':'+'",
    "                (X, 2)",
    "        in  %% Line 8",
    "             call 'erlang':'display'",
    "                 (X)",
    "end"]
    -- "            call 'io':'format'",
    -- "                ([126|[112]], [Y|[]])",

simpleModule = unlines [
    "module 'simple' ['main'/0]",
    "    attributes []",
    "'main'/0 =",
    "    fun () ->",
    "       let <X> =",
    "           call 'erlang':'date'",
    "               ()",
    "       in",
    "           call 'erlang':'display'",
    "               (X)",
    "end"]

-- Constr (Module (Atom "simple")
--         [Function (Atom "main",0)]
--         []
--         [FunDef (Constr (Function (Atom "main",0)))
--          (Constr (Lambda []
--                   (Exp (Constr (Let (["X"],
--                                      Exp (Constr
--                                           (ModCall (Exp (Constr (Lit (LAtom (Atom "mod")))),
--                                                     Exp (Constr (Lit (LAtom (Atom "read"))))) [])))
--                                 (Exp (Constr
--                                       (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))),
--                                                 Exp (Constr (Lit (LAtom (Atom "display")))))
--                                        [Exp (Constr (Var "X"))])
--                                      ))
--                                )))
--                  ))
--         ])



-- (Module (Atom "simple")
--  [Function (Atom "main",0)]
--  []
--  [FunDef (Constr (Function (Atom "main",0)))
--   (Constr (Lambda [] (Exp
--                       (Constr
--                        (Let (["X"],
--                              Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "random")))),
--                                                    Exp (Constr (Lit (LAtom (Atom "uniform"))))) [])))
--                         (Exp (Constr
--                               (Let (["Y"],
--                                     Exp (Constr (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))),
--                                                           Exp (Constr (Lit (LAtom (Atom "+")))))
--                                                  [Exp (Constr (Var "X")),
--                                                   Exp (Constr (Lit (LInt 2)))]
--                                                 )))
--                                (Exp (Constr
--                                      (ModCall (Exp (Constr (Lit (LAtom (Atom "erlang")))),
--                                                Exp (Constr (Lit (LAtom (Atom "display")))))
--                                       [Exp (Constr (Var "X"))])))))))))))])

data ErlTerm = ErlList [ErlTerm] |
               ErlTuple [ErlTerm] |
               ErlAtom String |
               ErlNum Integer |
               ErlFloat Double
             deriving Show

type VarTable = M.Map String ErlTerm
type ProcessDictionary = M.Map String ErlTerm
type ModTable = M.Map String (S.Ann S.Module)

data EvalCtx = ECtx VarTable
type ErlProcessState a = State ProcessDictionary a

newEvalCtx :: EvalCtx
newEvalCtx = ECtx M.empty

newProcDict :: ProcessDictionary
newProcDict = M.empty

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
eval eCtx (Let bindings exps) = do
  let eCtx' = setupFunctionContext eCtx bindings
  evalExps eCtx' exps

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

setupFunctionContext :: EvalCtx -> ([Var], Exps) -> EvalCtx
setupFunctionContext _ _ = undefined

findFn :: String -> Integer -> [FunDef] -> Maybe FunDef
-- findFn = undefined
findFn name arity funs =
  let
    aname = Atom name
    test = \(FunDef nm funs) ->
      let (Function ((Atom n), a)) = unann nm
      in (n == name) && (a == arity)
  in find test funs

-- | The main entry point.
main :: IO ()
main = do
  putStrLn "Welcome to FP Haskell Center!"
  putStrLn "Have a good day!"
  case P.parseModule twoLetMod of
    Left er ->  do
      putStrLn "Error"
      print er 
    Right m -> do
      -- @(Ann Module name _ funs)
      putStrLn $ show m
      putStrLn $ PP.prettyPrint m
      let Module modName exports attributes funs = unann m
      let Just (FunDef _ aexp) = findFn "main" 0 funs
      let exp = unann aexp
      print $ evalState (eval newEvalCtx exp) newProcDict
                                                          
                
