
module StandardFunctions where

import Expression
import Eval
import qualified Data.Map as Map
import Control.Monad.State

-- Standard functions we expose
blaiseArithmetic f  = do (BlaiseList args) <- getSymbol "..."
                         blaiseBinary f args

blaiseBinary :: (Integer->Integer->Integer) -> [Expr] -> BlaiseResult
blaiseBinary op args = do return $ foldl1 (blaiseBinaryAux op) args
	where blaiseBinaryAux op (BlaiseInt i) (BlaiseInt j) = BlaiseInt (i `op` j)

-- Equality
blaiseEq = do (BlaiseList args) <- getSymbol "..."
              return $ foldl1 (\(BlaiseInt a) (BlaiseInt b) -> BlaiseInt(if a == b then 1 else 0)) args

-- A function to modify the context
blaiseSetArgs = ["symbol", "value"]
blaiseSet = do [(BlaiseSymbol s), e] <- getSymbols blaiseSetArgs
               eval_e <- eval e
               updateSymbolInParent s eval_e
               return eval_e

-- Conditionals
blaiseIfArgs = ["condition", "expr1", "expr2"]
blaiseIf = do [condExpr, expr1, expr2] <- getSymbols blaiseIfArgs
              eval_cond <- eval condExpr
              if (0 `notEqual` eval_cond) then eval expr1
                                          else eval expr2
    where notEqual val1 (BlaiseInt val2) = val1 /= val2

-- Functions
blaiseFnArgs = ["args", "..."]
blaiseFn = do [(BlaiseList args), (BlaiseList body)] <- getSymbols blaiseFnArgs
              let newFn = do evalBody <- mapM eval body
                             return $ last evalBody
              return $ BlaiseFn newFn (map (\(BlaiseSymbol arg)->arg) args)

-- Our symbol table
initialCtx = Ctx (Map.fromList [("+", BlaiseFn (blaiseArithmetic (+)) ["..."]),
			   ("-", BlaiseFn (blaiseArithmetic (-)) ["..."]),
			   ("*", BlaiseFn (blaiseArithmetic (*)) ["..."]),
			   ("/", BlaiseFn (blaiseArithmetic div) ["..."]),
                           ("eq", BlaiseFn blaiseEq ["..."]),
			   ("set", BlaiseSpecial blaiseSet blaiseSetArgs),
                           ("if", BlaiseSpecial blaiseIf blaiseIfArgs),
                           ("fn", BlaiseSpecial blaiseFn blaiseFnArgs )
			  ]) Nothing

-- Helper
getSymbol sym = eval $ (BlaiseSymbol sym)
getSymbols syms = mapM getSymbol syms