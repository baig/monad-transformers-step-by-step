module Transformers where

import Data.Maybe
import qualified Data.Map as Map

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

eval0 :: Env -> Exp -> Value
eval0 _ (Lit i) = IntVal i
eval0 env (Var n) = fromJust $ Map.lookup n env -- when a variable is used which is not bound anywhere using a λ expression, the program will halt with an error message
eval0 env (Plus e1 e2) = let (IntVal i1) = eval0 env e1
                             (IntVal i2) = eval0 env e2
                          in IntVal (i1 + i2) -- Whenever one of the addition operands evaluates to a non-number, the pattern matching in the let expression will fail, also terminating the program with an error message.
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) = let funVal = eval0 env e1
                            intVal = eval0 env e2
                         in case funVal of
                            FunVal env' n body -> eval0 (Map.insert n intVal env') body -- Function application proceeds similar to addition, by first evaluating the function and the argument. The first ex- pression must evaluate to a functional value, whose body is then evaluated in the captured environment, extended with the binding of the function parameter to the argument value. The case expression used here to deconstruct the functional value introduces another error possibility.









