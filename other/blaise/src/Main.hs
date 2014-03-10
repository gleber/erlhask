
module Main where

import Eval
import Expression
import StandardFunctions
import System.IO
import Control.Monad.State
import Control.Monad.Error

main = do runErrorT (evalStateT repl initialCtx)
          return ()

repl = do liftIO $ putStr "> "
	  liftIO $ hFlush stdout
	  x <- liftIO $ getLine
	  if x == "(quit)" then return ()
			   else do expr <- parse x
                                   evaledExpr <- eval expr
				   liftIO $ putStrLn ((show evaledExpr))
				   repl
                                `catchError` (\e -> do liftIO $ putStrLn e
                                                       repl)
