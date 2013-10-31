module SalsaAst where

type Program = [DefCom]
data DefCom = Def Definition
            | Com Command
            deriving (Show, Eq)
data Definition = Viewdef Ident Expr Expr
                | Rectangle Ident Expr Expr Expr Expr Colour
                | Circle Ident Expr Expr Expr Colour
                | View Ident
                | Group Ident [Ident]
                deriving (Show, Eq)
data Command = Move [Ident] Pos
             | At Command Ident
             | Par Command Command
             deriving (Show, Eq)
data Pos = Abs Expr Expr
         | Rel Expr Expr
         deriving (Show, Eq)
data Expr = Plus Expr Expr
          | Minus Expr Expr
          | Const Integer
          | Xproj Ident
          | Yproj Ident
          deriving (Show, Eq)
data Colour = Blue | Plum | Red | Green | Orange
            deriving (Show, Eq)
type Ident = String


