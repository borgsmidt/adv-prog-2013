import Control.Exception (assert)
import SalsaParser
import SalsaAst
--import System.IO

-- passes a string and compares the AST to an expected program
run :: String -> Program -> String
run s p = assert (Right p == parseString s) (shw "pass" s)

-- passes a string and checks that an error occurs
err :: String -> Program -> String
err s _ = assert (isError $ parseString s) (shw "error (expected)" s)

-- indicates if a given result is an error
isError :: Either String Program -> Bool
isError (Left _) = True
isError (Right _) = False

-- formats test output
shw :: String -> String -> String
shw pre "" = pre ++ ": " ++ "<empty string>"
shw pre s = pre ++ ": " ++ s

-- main runs the test suite
main = do putStrLn "\n*** Checking Colour ***"
          putStrLn $ checkCol run "blue" Blue
          putStrLn $ checkCol run "plum" Plum
          putStrLn $ checkCol run "red" Red
          putStrLn $ checkCol run "green" Green
          putStrLn $ checkCol run "orange" Orange
          putStrLn $ checkCol err "violet" Red
          putStrLn $ checkCol err "greeen" Green
          putStrLn $ checkCol err "Blue" Blue
          putStrLn $ checkCol err "blue1" Blue

          putStrLn "\n*** Checking Prim ***"
          putStrLn $ checkExpr run "0" (Const 0)
          putStrLn $ checkExpr run "42" (Const 42)
          putStrLn $ checkExpr run "999999999" (Const 999999999)
          putStrLn $ checkExpr run "(((42)))" (Const 42)
          putStrLn $ checkExpr run "john . x" (Xproj "john")
          putStrLn $ checkExpr run "john . y" (Yproj "john")
          putStrLn $ checkExpr err "-5" (Const 0)
          putStrLn $ checkExpr err "-5" (Const 0)
          putStrLn $ checkExpr err "42.2" (Const 0)
          putStrLn $ checkExpr err ".8" (Const 0)

          putStrLn "\n*** Checking Expr ***"
          putStrLn $ checkExpr run "1 + 2" (Plus (Const 1) (Const 2))
          putStrLn $ checkExpr run "1 - 2" (Minus (Const 1) (Const 2))
          putStrLn $ checkExpr run "1 + 2 + 3" (Plus (Plus (Const 1) (Const 2)) (Const 3))
          putStrLn $ checkExpr run "1 - 2 - 3" (Minus (Minus (Const 1) (Const 2)) (Const 3))
          putStrLn $ checkExpr run "1 + 2 - 3" (Minus (Plus (Const 1) (Const 2)) (Const 3))
          putStrLn $ checkExpr run "1 - 2 + 3" (Plus (Minus (Const 1) (Const 2)) (Const 3))

          putStrLn "\n*** Checking Pos ***"
          putStrLn $ checkPos run "(0, 0)" (Abs (Const 0) (Const 0))
          putStrLn $ checkPos run "+ (0, 0)" (Rel (Const 0) (Const 0))

          putStrLn "\n*** Checking SIdents ***"
          putStrLn $ checkSIdents run "a" ["a"]
          putStrLn $ checkSIdents run "aBC" ["aBC"]
          putStrLn $ checkSIdents run "a12T" ["a12T"]
          putStrLn $ checkSIdents run "aa bb cc" ["aa", "bb", "cc"]
          putStrLn $ checkSIdents err "1abc" []
          putStrLn $ checkSIdents err "Abc" []
          putStrLn $ checkSIdents err "_abc" []
          putStrLn $ checkSIdents err "_" []
          mapM_ putStrLn $ (map $ flip (checkSIdents err) []) reserved

          putStrLn "\n*** Checking VIdents ***"
          putStrLn $ checkVIdents run "A" ["A"]
          putStrLn $ checkVIdents run "Abc" ["Abc"]
          putStrLn $ checkVIdents run "A12t" ["A12t"]
          putStrLn $ checkVIdents run "AA BB CC" ["AA", "BB", "CC"]
          putStrLn $ checkVIdents err "1Abc" []
          putStrLn $ checkVIdents err "aBC" []
          putStrLn $ checkVIdents err "_ABC" []
          putStrLn $ checkVIdents err "_" []

          putStrLn "\n*** Checking Command ***"
          putStrLn $ checkCommand run "a->(0, 0)" (Move ["a"] (Abs (Const 0) (Const 0)))
          putStrLn $ checkCommand run "a->(0, 0)@V" (At (Move ["a"] (Abs (Const 0) (Const 0))) "V")
          putStrLn $ checkCommand run "a->(0, 0)@V@W" (At (At (Move ["a"] (Abs (Const 0) (Const 0))) "V") "W")
          putStrLn $ checkCommand run "a->(0, 0)||b->(0, 0)" (Par (Move ["a"] (Abs (Const 0) (Const 0)))
                                                                  (Move ["b"] (Abs (Const 0) (Const 0))))
          putStrLn $ checkCommand run "a->(0, 0)||b->(0, 0)||c->(0,0)" (Par (Par (Move ["a"] (Abs (Const 0) (Const 0)))
                                                                                 (Move ["b"] (Abs (Const 0) (Const 0))))
                                                                            (Move ["c"] (Abs (Const 0) (Const 0))))
          putStrLn $ checkCommand run "a->(0, 0)||b->(0, 0)@V" (Par (Move ["a"] (Abs (Const 0) (Const 0)))
                                                                    (At (Move ["b"] (Abs (Const 0) (Const 0))) "V"))
          putStrLn $ checkCommand run "{{{a->(0, 0)}}}" (Move ["a"] (Abs (Const 0) (Const 0)))

          putStrLn "\n*** Checking Definition ***"
          putStrLn $ checkDefinition run "viewdef V 0 0" (Viewdef "V" (Const 0) (Const 0))
          putStrLn $ checkDefinition run "rectangle r 0 0 0 0 blue" (Rectangle "r" (Const 0) (Const 0) (Const 0) (Const 0) Blue)
          putStrLn $ checkDefinition run "circle c 0 0 0 blue" (Circle "c" (Const 0) (Const 0) (Const 0) Blue)
          putStrLn $ checkDefinition run "view V" (View "V")
          putStrLn $ checkDefinition run "group G [X Y Z]" (Group "G" ["X", "Y", "Z"])
          putStrLn $ checkDefinition err "view1 V" (View "V")

          putStrLn "\n*** Checking Program ***"
          putStrLn $ err "" []

          putStrLn $ run ("viewdef Default 400 400\n" ++
                          "rectangle box 10 400 20 20 green\n" ++
                          "box -> (10, 200)\n" ++
                          "box -> +(100, 0)\n" ++
                          "box -> (110,400)\n" ++
                          "box -> +(0-100, 0)\n")

                       [ Def (Viewdef "Default" (Const 400) (Const 400))
                       , Def (Rectangle "box" (Const 10) (Const 400)
                                              (Const 20) (Const 20) Green)
                       , Com (Move ["box"] (Abs (Const 10) (Const 200)))
                       , Com (Move ["box"] (Rel (Const 100) (Const 0)))
                       , Com (Move ["box"] (Abs (Const 110) (Const 400)))
                       , Com (Move ["box"] (Rel (Minus (Const 0) (Const 100)) (Const 0)))]

          putStrLn $ run ("viewdef One 500 500\n" ++
                          "viewdef Two 400 400\n" ++
                          "group Both [One Two]\n" ++
                          "view Both\n" ++
                          "rectangle larry 10 350 20 20 blue\n" ++
                          "rectangle fawn 300 350 15 25 plum\n\n" ++
                          "view Two\n" ++
                          "larry -> (300, 350) || fawn -> (10,350)\n\n" ++
                          "view Both\n" ++
                          "larry fawn -> +(0, 0 - 300)")

                       [ Def (Viewdef "One" (Const 500) (Const 500))
                       , Def (Viewdef "Two" (Const 400) (Const 400))
                       , Def (Group "Both" ["One","Two"])
                       , Def (View "Both")
                       , Def (Rectangle "larry" (Const 10) (Const 350)
                                                (Const 20) (Const 20) Blue)
                       , Def (Rectangle "fawn" (Const 300) (Const 350)
                                               (Const 15) (Const 25) Plum)
                       , Def (View "Two")
                       , Com (Par (Move ["larry"] (Abs (Const 300) (Const 350)))
                                  (Move ["fawn"] (Abs (Const 10) (Const 350))))
                       , Def (View "Both")
                       , Com (Move ["larry","fawn"]
                                   (Rel (Const 0) (Minus (Const 0) (Const 300))))]

          putStrLn "\n*** All tests completed successfully ***\n"

    where
      checkCol f s c = f ("circle c 0 0 0 " ++ s) [Def (Circle "c" (Const 0) (Const 0) (Const 0) c)]
      checkExpr f s e = f ("a -> (" ++ s ++ ", " ++ s ++ ")") [Com (Move ["a"] (Abs e e))]
      checkPos f s p = f ("a -> " ++ s) [Com (Move ["a"] p)]
      checkSIdents f s ids = f (s ++ " -> (0, 0)") [Com (Move ids (Abs (Const 0) (Const 0)))]
      checkVIdents f s ids = f ("group V [" ++ s ++ "]") [Def (Group "V" ids)]
      checkCommand f s c = f s [Com c]
      checkDefinition f s d = f s [Def d]
