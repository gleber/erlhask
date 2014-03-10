
module Expression where

import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Error

-- Our Lisp expression
data Expr = BlaiseInt Integer |
	    BlaiseSymbol String |
	    BlaiseFn BlaiseFunction FunctionSignature |
	    BlaiseSpecial BlaiseFunction FunctionSignature |
	    BlaiseList [Expr]

-- The function type
type FunctionSignature = [String]
type BlaiseFunction = BlaiseResult

-- Context in which expressions will be evaluated
type SymbolTable = Map.Map String Expr
data Context = Ctx SymbolTable (Maybe Context)

-- Helper context functions
updateSymbol s eval_e = modify (\(Ctx sym_table parentCtx)->(Ctx (Map.insert s eval_e sym_table)) parentCtx)
updateSymbolInParent s eval_e = modify (\(Ctx sym_table parent_ctx)->(Ctx sym_table (updatedCtx parent_ctx)))
    where updatedCtx (Just (Ctx sym_table ctx)) = (Just (Ctx (Map.insert s eval_e sym_table) ctx))
pushContext ctx = Ctx Map.empty (Just ctx)
popContext ctx@(Ctx _ Nothing) = ctx
popContext (Ctx _ (Just parentCtx)) = parentCtx

-- A state monad that holds a context and an evaluation result
type BlaiseError = ErrorT String IO
type BlaiseResult = StateT Context BlaiseError Expr

-- Printing the expression
instance Show Expr where
	show (BlaiseInt x) = show x
	show (BlaiseSymbol x) = x
	show (BlaiseFn _ _) = "<function>"
	show (BlaiseSpecial _ _) = "<special-form>"
	show (BlaiseList x) = "(" ++ unwords (map show x) ++ ")"

-- Parsing the expression
parseInteger = do sign <- option "" (string "-")
		  number <- many1 digit
		  return $ BlaiseInt (read (sign++number))

parseSymbol = do f <- firstAllowed
		 r <- many (firstAllowed <|> digit)
		 return $ BlaiseSymbol (f:r)
	where firstAllowed = oneOf "+-*/" <|> letter

parseExprAux = (try parseInteger) <|> (try parseSymbol) <|> (try parseList)

parseList = do char '(' ; skipMany space
	       x <- parseExprAux `sepEndBy` (many1 space)
	       char ')'
	       return $ BlaiseList x

parseExpr = do skipMany space
	       x <- parseExprAux
	       skipMany space ; eof
	       return x

parse :: String -> BlaiseResult
parse source = case (Text.ParserCombinators.Parsec.parse parseExpr "" source) of
		 Right x -> return x
		 Left e -> throwError $ show e
