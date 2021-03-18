
import Data.IORef
import Evaluator

-- use monads to simulate state in Haskell
-- State Monad: hide arbitrary state within the monad and pass it around behind the scenes

--- . For a simple top-level environment, we could get away with [(String, LispVal)], storing mappings from variable names to values. However, when we start dealing with function calls, these mappings become a stack of nested environments, arbitrarily deep

-- Instead, we use a feature called state threads, letting Haskell manage the aggregate state for us.

-- There are two flavors of state threads: the ST monad creates a stateful computation that can be executed as a unit, without the state escaping to the rest of the program. The IORef module lets you use stateful variables within the IO monad. Since our state has to be interleaved with IO anyway (it persists between lines in the REPL, and we will eventually have IO functions within the language itself), we'll be using IORefs



type Env = IORef [(String, IORef LispVal)]
-- need IORefs for both the list itself and for individual values because there are two ways that the program can mutate the environment. 
    --  It might use set! to change the value of an individual variable, a change visible to any function that shares that environment (Scheme allows nested scopes, so a variable in an outer scope is visible to all inner scopes).
    
    -- or use define

-- IORefs can only be used within the IO monad ??

nullEnv :: IO Env
nullEnv = newIORef []

-- monad transformer that lets you combine the functionality of multiple monads

type IOThrowsError = ExceptT LispError IO


-- use lifting to bring values of the lower type IO into the combined Monad


-- need this because there is not function to bring a value pf tje untransformed upper type into the combined monad
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val
--methods in typeclasses resolve based on the type of the expression, so throwError and return (members of MonadError and Monad, respectively) take on their IOThrowsError definitions

runIOThrows :: IOThr0wsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return extractValue


isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do 
                        env <- liftIO $ readIORef envRef
                        maybe (throwError $ UnboundVar "Getting an unbound variable" var) (liftIO . readIORef) (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
                            env <- liftIO $ readIORef envRef
                            maybe (throwError $ UnboundVar "Setting an unbound variable" var) (liftIO . (flip writeIORef value)) (lookup var env)
                            return value

                            