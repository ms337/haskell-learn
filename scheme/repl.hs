module Main where 
import System.IO
import System.Environment
import SchemeParser
import Evaluator
import Control.Monad


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

{-
evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr =  evalString expr >>= putStrLn
-}

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr


until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
    else action result >> until_ pred prompt action

{-
runRepl :: IO ()
runRepl = until_ (== ":q") (readPrompt "Lisp>>> ") evalAndPrint
-}


runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (=="quit") (readPrompt "Lisp>>> ") . evalAndPrint

main :: IO ()
main = do args <- getArgs
          case length args of
               0 -> runRepl
               1 -> runOne $ args !! 0
               otherwise -> putStrLn "Program takes only 0 or 1 argument"