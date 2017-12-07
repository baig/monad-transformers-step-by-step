module Main where

import Transform
import qualified Data.Map as Map

-- 12 + ((λx → x)(4 + 2))
exampleExp1 = (Lit 12) `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
exampleExp2 = (Plus (Lit 1) (Abs "x" (Var "x")))
exampleExp3 = (App (Lit 4) (Lit 4))

main = do
    runEval Map.empty 0 (eval exampleExp1)
    runEval Map.empty 0 (eval exampleExp2)
    runEval Map.empty 0 (eval exampleExp3)
