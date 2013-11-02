module SalsaTest

where

import SalsaAst
import Gpx
import qualified Data.Map as M

data Environment = Environment {
      definitions :: M.Map Ident Definition
    , activeViews :: [Ident]
    , frameRate :: Integer
    }

data State = State {
      positions :: M.Map (Ident, Ident) Pos
    , frames :: [Frame]
    }

{-data Context = Context {
      environment :: Environment
    , state :: State
    }
-}
type Context = Int

data Salsa a = Salsa (Context -> (a, Context))
apply (Salsa f) ctx = f ctx
instance Monad Salsa where
    return x = Salsa $ \context -> (x, context)
    m >>= f = Salsa $ \context -> let (x, context') = apply m context
                                         in apply (f x) context'

data Shape = Rectangle {
      llx :: Integer
    , lly :: Integer
    , width :: Integer
    , height :: Integer
    , col :: Colour
    } | Circle {
      x :: Integer
    , y :: Integer
    , r :: Integer
    , col :: Colour
    } deriving (Show, Eq)
