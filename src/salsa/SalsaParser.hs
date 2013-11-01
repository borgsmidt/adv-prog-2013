--
-- Skeleton for Salsa parser
-- To be used at the exam for Advanced Programming, B1-2013
--

module SalsaParser where

import SalsaAst
import SimpleParse
import Data.Char (isLetter, isDigit)

-- A string is used to signal an error
type Error = String

reserved :: [String]
reserved = ["viewdef", "rectangle", "circle", "view", "group"]

-- top-level parsing function that returns a program, or a string in case of failure
parseString :: String -> Either Error Program
parseString input = let res = parse (do r <- program
                                        eof
                                        return r) input
                    in case res of
                         [] -> Left ("unable to parse input: " ++ input)
                         (r,_):_ -> Right r

-- top-level parsing function that reads its input from a file
-- parseFile :: FilePath -> IO (Either Error Program)

-- Program parser
program :: Parser Program
program = defComs

-- DefComs parser
defComs :: Parser [DefCom]
defComs = do d <- defCom
             ds <- defComs_
             return (d:ds)

-- DefComs* parser
defComs_ :: Parser [DefCom]
defComs_ = (do d <- defCom
               ds <- defComs_
               return (d:ds))
           <|> return []

-- DefCom parser
defCom :: Parser DefCom
defCom = (do d <- def
             return (Def d))
         <|> (do c <- com
                 return (Com c))

-- Definition parser
def :: Parser Definition
def = (do symbol "viewdef"
          v <- vIdent
          e0 <- expr
          e1 <- expr
          return (Viewdef v e0 e1))
      <|> (do symbol "rectangle"
              s <- sIdent
              e0 <- expr
              e1 <- expr
              e2 <- expr
              e3 <- expr
              c <- col
              return (Rectangle s e0 e1 e2 e3 c))
      <|> (do symbol "circle"
              s <- sIdent
              e0 <- expr
              e1 <- expr
              e2 <- expr
              e3 <- expr
              c <- col
              return (Circle s e0 e1 e2 c))
      <|> (do symbol "view"
              v <- vIdent
              return (View v))
      <|> (do symbol "group"
              v <- vIdent
              schar '['
              vs <- vIdents
              schar ']'
              return (Group v vs))

-- Command parser
com :: Parser Command
com = com1 >>= com_

-- Command* parser
com_ :: Command -> Parser Command
com_ c0 = (do symbol "||"
              c1 <- com1
              com_ (Par c0 c1))
          <|> return c0

-- Command1 parser
com1 :: Parser Command
com1 = com2 >>= com1_

-- Command1* parser
com1_ :: Command -> Parser Command
com1_ c = (do schar '@'
              v <- vIdent
              com_ (At c v))
          <|> return c

-- Command2 parser
com2 :: Parser Command
com2 = (do ss <- sIdents
           symbol "->"
           p <- pos
           return (Move ss p))
       <|> (do schar '{'
               c <- com
               schar '}'
               return c)

-- VIdents parser
vIdents :: Parser [Ident]
vIdents = do v <- vIdent
             vs <- vIdents_
             return (v:vs)

-- VIdents* parser
vIdents_ :: Parser [Ident]
vIdents_ = (do v <- vIdent
               vs <- vIdents_
               return (v:vs))
           <|> return []

-- SIdents parser
sIdents :: Parser [Ident]
sIdents = do s <- sIdent
             ss <- sIdents_
             return (s:ss)

-- SIdents* parser
sIdents_ :: Parser [Ident]
sIdents_ = (do s <- sIdent
               ss <- sIdents_
               return (s:ss))
           <|> return []

-- Pos parser
pos :: Parser Pos
pos = (do schar '('
          e0 <- expr
          schar ','
          e1 <- expr
          schar ')'
          return (Abs e0 e1))
      <|> (do schar '+'
              schar '('
              e0 <- expr
              schar ','
              e1 <- expr
              schar ')'
              return (Rel e0 e1))

-- Expr parser
expr :: Parser Expr
expr = prim >>= expr_

-- Expr* parser
expr_ :: Expr -> Parser Expr
expr_ e0 = (do schar '+'
               e1 <- prim
               expr_ (Plus e0 e1))
          <|> (do schar '-'
                  e1 <- prim
                  expr_ (Minus e0 e1))
          <|> return e0

-- Prim parser
prim :: Parser Expr
prim = (do i <- integer
           return (Const i))
       <|> (do s <- sIdent
               schar '.'
               proj s)
       <|> (do schar '('
               e <- expr
               schar ')'
               return e)

-- This parser function handles the coordinate selection in Prim expressions
proj :: Ident -> Parser Expr
proj s = (do schar 'x'
             return (Xproj s))
         <|> (do schar 'y'
                 return (Yproj s))

-- Colour parser
col :: Parser Colour
col = (symbol "blue" >> return Blue)
      <|> (symbol "plum" >> return Plum)
      <|> (symbol "red" >> return Red)
      <|> (symbol "green" >> return Green)
      <|> (symbol "orange" >> return Orange)

-- identifiers
vIdent :: Parser Ident
vIdent = token (do c <- letter
                   cs <- letdigs
                   if (c:cs) `elem` reserved then reject
                   else return (c:cs))
    where letter = satisfy isLetter
          letdigs = many (letter <|> num <|> char '_')
          num = satisfy isDigit
          reserved = ["where", "refv", "refh", "rot", "width", "height"]

sIdent :: Parser Ident
sIdent = token (do c <- letter
                   cs <- letdigs
                   if (c:cs) `elem` reserved then reject
                   else return (c:cs))
    where letter = satisfy isLetter
          letdigs = many (letter <|> num <|> char '_')
          num = satisfy isDigit
          reserved = ["where", "refv", "refh", "rot", "width", "height"]

integer :: Parser Integer
integer = token (do intpart <- digits
                    decsep <- string "."
                    decpart <- digits
                    return (read (intpart ++ decsep ++ decpart)))
          <|> token (do intpart <- digits
                        return (read intpart))
    where digits = many1 num
          num = satisfy isDigit
