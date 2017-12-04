module Main where

import Transformers
import qualified Data.Map as Map

-- 12 + ((λx → x)(4 + 2))
exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

main :: IO ()
main = putStrLn $ show $ eval0 Map.empty exampleExp
