module Main where

import Transform
import qualified Data.Map as Map

-- 12 + ((λx → x)(4 + 2))
exampleExp1 = (Abs "x" (Var "x")) `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
exampleExp2 = (Plus (Lit 1) (Abs "x" (Var "x")))
exampleExp3 = (App (Lit 4) (Lit 4))

main :: IO ()
main = do
    putStrLn $ show $ runEval Map.empty (eval exampleExp1)
    putStrLn $ show $ runEval Map.empty (eval exampleExp2)
    putStrLn $ show $ runEval Map.empty (eval exampleExp3)
