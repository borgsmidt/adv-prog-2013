import Control.Exception (assert)
import SalsaParser
import SalsaAst

-- passes a string and compares the AST to an expected program
run :: String -> Program -> String
run s p = assert (Right p == parseString s) (shw "pass" s)

-- passes a string and checks that an error occurs
err :: String -> String
err s = assert (isError $ parseString s) (shw "error" s)

-- indicates if a given result is an error
isError :: Either String Program -> Bool
isError (Left _) = True
isError (Right _) = False

-- formats test output
shw :: String -> String -> String
shw pre "" = pre ++ ": " ++ "<empty string>"
shw pre s = pre ++ ": " ++ s

-- main runs the test suite
main = do putStrLn $ err ""

          putStrLn "\n*** Checking Prim ***"
          putStrLn $ checkExpr "0" (Const 0)
          putStrLn $ checkExpr "42" (Const 42)
          putStrLn $ checkExpr "(((42)))" (Const 42)
          putStrLn $ checkExpr "john . x" (Xproj "john")
          putStrLn $ checkExpr "john . y" (Yproj "john")

          putStrLn "\n*** Checking Expr ***"
          putStrLn $ checkExpr "1 + 2" (Plus (Const 1) (Const 2))
          putStrLn $ checkExpr "1 - 2" (Minus (Const 1) (Const 2))
          putStrLn $ checkExpr "1 + 2 + 3" (Plus (Plus (Const 1) (Const 2)) (Const 3))
          putStrLn $ checkExpr "1 - 2 - 3" (Minus (Minus (Const 1) (Const 2)) (Const 3))
          putStrLn $ checkExpr "1 + 2 - 3" (Minus (Plus (Const 1) (Const 2)) (Const 3))
          putStrLn $ checkExpr "1 - 2 + 3" (Plus (Minus (Const 1) (Const 2)) (Const 3))

          putStrLn "\n*** Checking Pos ***"
          putStrLn $ checkPos "(0, 0)" (Abs (Const 0) (Const 0))
          putStrLn $ checkPos "+ (0, 0)" (Rel (Const 0) (Const 0))
    where
      checkExpr s e = run ("a -> (" ++ s ++ "," ++ s ++ ")") [Com (Move ["a"] (Abs e e))]
      checkPos s p = run ("a -> " ++ s) [Com (Move ["a"] p)]
