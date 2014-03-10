
module Eval where

import Expression
import qualified Data.Map as Map
import Data.List
import Control.Monad.State
import Control.Monad.Error

-- Evaluating Lisp expression
eval :: Expr -> BlaiseResult
eval (BlaiseInt n) = return (BlaiseInt n)
eval (BlaiseFn f args) = return (BlaiseFn f args)
eval (BlaiseSpecial f args) = return (BlaiseSpecial f args)
eval (BlaiseSymbol s) = do context <- get
                           lookupSymbol context
    where lookupSymbol (Ctx sym_table parentCtx) =
              if s `Map.member` sym_table == True
              then return (sym_table Map.! s)
              else case parentCtx of
                     Nothing -> throwError ("Symbol " ++ s ++ " is unbound.")
                     (Just parent) -> lookupSymbol parent

eval (BlaiseList (x:xs)) = do fn <- eval x
			      apply fn
	where apply (BlaiseSpecial f expectedArgs) = apply' expectedArgs xs f
	      apply (BlaiseFn f expectedArgs) = do args <- mapM eval xs
                                                   apply' expectedArgs args f
              apply' expectedArgs args f = do modify pushContext
                                              applyArgsToContext expectedArgs args
                                              result <- f
                                              modify popContext
                                              return result
              applyArgsToContext ("...":_) args = do updateSymbol "..." (BlaiseList args)
              applyArgsToContext (earg:expectedArgs) (arg:args) = do updateSymbol earg arg
                                                                     applyArgsToContext expectedArgs args
              applyArgsToContext [] _ = return ()
