--
-- Skeleton for Salsa interpreter
-- To be used at the exam for Advanced Programming, B1-2013
--

module SalsaInterp
       -- (Position, interpolate, runProg)
where

import SalsaAst
import Gpx
import qualified Data.Map as M

type Position = (Integer, Integer)
interpolate :: Integer -> Position -> Position -> [Position]
-- TODO
interpolate rate fromPos toPos = [toPos]

type View = (Ident, Integer, Integer)
type Group = (Ident, [Ident])
data Shape = Rect {
      llx :: Integer
    , lly :: Integer
    , width :: Integer
    , height :: Integer
    , col :: Colour
    } | Circ {
      x :: Integer
    , y :: Integer
    , r :: Integer
    , col :: Colour
    } deriving (Show, Eq)

data State = State {
      positions :: M.Map (Ident, Ident) Pos
    , frameSets :: [[Frame]]
    }

data Context = Context {
      views :: M.Map Ident View
    , shapes :: M.Map Ident Shape
    , groups :: M.Map Ident [Ident]
    , activeViews :: [Ident]
    , frameRate :: Integer
    , state :: State
    }

-- TODO Clean this up
baseState = State M.empty []
baseContext = Context M.empty M.empty M.empty [] 1 baseState

-- Changes the context locally for the command
local :: (Context -> Context) -> SalsaCommand a -> SalsaCommand a
local f m = SalsaCommand $ \context -> runSC m (f context)

animation :: Context -> Animation
-- TODO Needs to merge frame sets
animation context = (M.elems $ views context, [])

moveShapes :: Context -> [Ident] -> Pos -> State
-- TODO
moveShapes context idents pos = state context

addView :: Ident -> View -> Context -> Context
addView ident view context = let newViews = M.insert ident view (views context)
                             in context { views = newViews }

addShape :: Ident -> Shape -> Context -> Context
addShape ident shape context = let newShapes = M.insert ident shape (shapes context)
                               in context { shapes = newShapes }

addGroup :: Ident -> [Ident] -> Context -> Context
addGroup ident idents context = let newGroups = M.insert ident idents (groups context)
                                in context { groups = newGroups }

setActive :: Ident -> Context -> Context
-- TODO
setActive ident context = context

-- When talking about this type, point out that no error handling is necessary
-- This type reflects that running a command cannot update the environment,
-- just the state
-- The type captures the effect of a command in a given context
newtype SalsaCommand a = SalsaCommand { runSC :: Context -> (a, State) }
instance Monad SalsaCommand where
    return x = SalsaCommand $ \context -> (x, state context)
    m >>= f = SalsaCommand $ \context -> let (x, st) = runSC m context
                                         in runSC (f x) context { state = st }


newtype Salsa a = Salsa { runSalsa :: Context -> (a, Context) }
instance Monad Salsa where
    return x = Salsa $ \context -> (x, context)
    m >>= f = Salsa $ \context -> let (x, context') = runSalsa m context
                                  in runSalsa (f x) context'


command :: Command -> SalsaCommand ()
command (At com ident) = local (setActive ident) $ command com
command (Par com0 com1) = command com0 >> command com1 >> mergeTwoFrameSets ()
command (Move idents pos) =
    SalsaCommand $ \context -> let state = moveShapes context idents pos
                               in ((), state)


mergeTwoFrameSets :: a -> SalsaCommand a
-- TODO Needs to handle duplicates because of stationary shapes
mergeTwoFrameSets x =
    SalsaCommand $ \context -> let st = state context
                                   (fs0:fs1:rest) = frameSets st
                                   merged = zipWith (++) fs0 fs1
                               in (x, st { frameSets = (merged:rest) })


{-let shapes 
                                   current = currentPos context idents
                                   new = newPos current pos

[[0, 0, 0], [1, 1, 1]]
[[2, 2, 2], [2, 2, 2]]

get current positions for idents
calculate new positions
get positions for other shapes
do interpolation
generate graphics instructions


 env = environment context
                                   st = state context
                                   views = activeViews env

-}

-- TODO
eval :: Context -> Expr -> Integer
eval context expr = 42

colorName :: Colour -> String
colorName Blue = "blue"
colorName Plum = "plum"
colorName Red = "red"
colorName Green = "green"
colorName Orange = "orange"

liftC :: SalsaCommand a -> Salsa a
liftC sc = Salsa $ \context -> let (x, st) = runSC sc context
                               in (x, context { state = st })

definition :: Definition -> Salsa ()
definition (Viewdef ident wExpr hExpr) =
    Salsa $ \context -> let width = eval context wExpr
                            height = eval context hExpr
                        in ((), addView ident (ident, width, height) context)
definition (Rectangle ident llxExpr llyExpr wExpr hExpr col) =
    Salsa $ \context -> let  llx = eval context llxExpr
                             lly = eval context llyExpr
                             width = eval context wExpr
                             height = eval context hExpr
                             rect = Rect llx lly width height col
                        in ((), addShape ident rect context)
definition (Circle ident xExpr yExpr rExpr col) =
    Salsa $ \context -> let  x = eval context xExpr
                             y = eval context yExpr
                             r = eval context rExpr
                             circle = Circ x y r col
                        in ((), addShape ident circle context)
definition (View ident) = Salsa $ \context -> ((), setActive ident context)
definition (Group ident idents) = Salsa $ \context -> ((), addGroup ident idents context)

defCom :: DefCom -> Salsa ()
defCom (Def def) = definition def
defCom (Com com) = liftC $ command com


runProg :: Integer -> Program -> Animation
runProg framerate program =
    let salsas = map defCom program
        ((), context) = runSalsa (sequence_ salsas) baseContext
    in animation context
