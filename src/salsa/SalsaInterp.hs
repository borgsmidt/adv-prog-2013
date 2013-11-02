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

type ViewDef = (Ident, Integer, Integer)
type GroupDef = (Ident, [Ident])
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

data Environment = Environment {
      views :: [ViewDef]
    , groups :: [GroupDef]
    , shapes :: M.Map Ident Shape
    , definitions :: M.Map Ident Definition
    , activeViews :: [Ident]
    , frameRate :: Integer
    }

data State = State {
      positions :: M.Map (Ident, Ident) Pos
    , frameSets :: [[Frame]]
    }

data Context = Context {
      environment :: Environment
    , state :: State
    }

baseEnvironment = Environment [] [] M.empty M.empty [] 1
baseState = State M.empty []
baseContext = Context baseEnvironment baseState

-- Changes the context locally for the command
local :: (Context -> Context) -> SalsaCommand a -> SalsaCommand a
local f m = SalsaCommand $ \context -> runSC m (f context)

setActive :: Ident -> Context -> Context
-- TODO
setActive ident context = context

animation :: Context -> Animation
-- TODO Needs to merge frame sets
animation context = ([], [])

moveShapes :: Context -> [Ident] -> Pos -> State
-- TODO
moveShapes context idents pos = state context

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

liftC :: SalsaCommand a -> Salsa a
liftC scom = Salsa $ \context -> let (x, st) = runSC scom context
                                 in (x, context { state = st })
                


definition :: Definition -> Salsa ()
-- TODO
definition def = return ()

defCom :: DefCom -> Salsa ()
defCom (Def def) = definition def
defCom (Com com) = liftC $ command com


runProg :: Integer -> Program -> Animation
runProg framerate program =
    let salsas = map defCom program
        ((), context) = runSalsa (sequence_ salsas) baseContext
    in animation context
