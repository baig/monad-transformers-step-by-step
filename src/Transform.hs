module Transform where

import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

type Name = String

type Env = Map.Map Name Value

-- the programs which are to be evaluated will be made up of the Exp data type
data Exp = Lit Integer
         | Var Name
         | Plus Exp Exp
         | Abs Name Exp
         | App Exp Exp
         deriving (Show)

-- the results will be from the Value type
data Value = IntVal Integer
           | FunVal Env Name Exp -- closure
           deriving (Show)

-- In order to use monad transformers, it is necessary to express functions in monadic style. That means that the programmer needs to impose sequencing on all monadic operations using do notation, and to use the return function in order to specify the result of a function.
-- We define a monad in which the evaluator will be defined. The following type synonym defines Eval α as a synonym for the type Identity α. Identity is a monad imported from Control.Monad.Identity, which is perhaps the simplest monad imaginable: it defines the standard return and >= operations for constructing operations in the monad, and additionally a function runIdentity to execute such operations. Other than that, the identity monad has no effect.
type Eval a = ReaderT Env (ErrorT String (WriterT [String] (StateT Integer IO))) a

-- For readability, we also define a function runEval1, which simply calls runIdentity.
runEval :: Env -> Integer -> Eval a -> IO ((Either String a, [String]), Integer)
runEval env st ev = runStateT (runWriterT $ runErrorT $ runReaderT ev env) st

tick :: (Num s, MonadState s m) => m ()
tick = do
      st <- get
      put (st + 1)

eval :: Exp -> Eval Value
eval (Lit i) = do tick
                  liftIO $ print i
                  return $ IntVal i
eval (Var n) = do tick
                  tell [n]
                  env <- ask
                  case Map.lookup n env of
                        Nothing -> throwError $ "unbound variable" ++ n
                        Just v -> return v
eval (Plus e1 e2) = do  tick
                        e1' <- eval e1
                        e2' <- eval e2
                        case (e1', e2') of
                              (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2) -- Whenever one of the addition operands evaluates to a non-number, the pattern matching in the let expression will fail, also terminating the program with an error message.
                              _ -> throwError "type error"
eval (Abs n e) = do tick
                    env <- ask
                    return $ FunVal env n e
eval (App e1 e2) = do   tick
                        funVal <- eval e1
                        intVal <- eval e2
                        case funVal of
                              FunVal env' n body -> local (const $ Map.insert n intVal env') (eval body) -- Function application proceeds similar to addition, by first evaluating the function and the argument. The first ex- pression must evaluate to a functional value, whose body is then evaluated in the captured environment, extended with the binding of the function parameter to the argument value. The case expression used here to deconstruct the functional value introduces another error possibility.
                              _ -> throwError "type error"









