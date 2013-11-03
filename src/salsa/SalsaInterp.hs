--
-- Skeleton for Salsa interpreter
-- To be used at the exam for Advanced Programming, B1-2013
--

module SalsaInterp
       -- (Position, interpolate, runProg)
where

import SalsaAst
import Gpx
import Data.List((\\))
import qualified Data.Map as M

type Position = (Integer, Integer)
interpolate :: Integer -> Position -> Position -> [Position]
-- TODO
interpolate rate fromPos toPos = [toPos]

data Shape = Rect {
      llx :: Integer
    , lly :: Integer
    , width :: Integer
    , height :: Integer
    , col :: Colour
    , vs :: [ViewT]
    } | Circ {
      x :: Integer
    , y :: Integer
    , r :: Integer
    , col :: Colour
    , vs :: [ViewT]
    } deriving (Show, Eq)

type FrameSets = [[Frame]]
data State = State {
      positions :: M.Map (Ident, Ident) Pos
    , frameSets :: FrameSets
    }

-- 'T' suffix is just to distinguish from Salsa AST constructors
type ViewT = (Ident, Integer, Integer)
type GroupT = (Ident, [ViewT])
type ViewMap = M.Map Ident ViewT
type ShapeMap = M.Map Ident Shape
type GroupMap = M.Map Ident GroupT
data Context = Context {
      views :: ViewMap
    , shapes :: ShapeMap
    , groups :: GroupMap
    , activeViews :: [ViewT]
    , frameRate :: Integer
    , state :: State
    }

blankFrame = []
-- TODO Clean this up
baseState = State M.empty [[blankFrame]]
baseContext = Context M.empty M.empty M.empty [] 1 baseState

updateViews :: (ViewMap -> ViewMap) -> Context -> Context
updateViews f = \context -> context { views = f (views context) }

updateShapes :: (ShapeMap -> ShapeMap) -> Context -> Context
updateShapes f = \context -> context { shapes = f (shapes context) }

updateGroups :: (GroupMap -> GroupMap) -> Context -> Context
updateGroups f = \context -> context { groups = f (groups context) }

updateState :: (State -> State) -> Context -> Context
updateState f = \context -> context { state = f (state context) }

updateFrameSets :: (FrameSets -> FrameSets) -> State -> State
updateFrameSets f = \state -> state { frameSets = f (frameSets state) }

-- the key frame of a given context is always the latest frame in the latest frame set
-- and a context always has at least a key frame, so we can assume it is there when
-- pattern matching
updateKeyFrame :: (Frame -> Frame) -> FrameSets -> FrameSets
updateKeyFrame f = \((kf:set):sets) -> ((f kf):set):sets

-- Changes the context locally for the command
local :: (Context -> Context) -> SalsaCommand a -> SalsaCommand a
local f m = SalsaCommand $ \context -> runSC m (f context)

animation :: Context -> Animation
-- TODO Needs to merge frame sets, reverse, etc.
animation context =
    let vs = M.elems $ views context
        fs = reverse . concat . frameSets $ state context
    in (vs , fs)

moveShapes :: Context -> [Ident] -> Pos -> State
-- TODO
moveShapes context idents pos = state context
{-get current positions for idents
calculate new positions
get positions for other shapes
do interpolation
generate graphics instructions
-}
addView :: Ident -> ViewT -> Context -> Context
addView ident view context =
    setActive ident $ updateViews (M.insert ident view) context

addShape :: Ident -> Shape -> Context -> Context
addShape ident shape context =
    writeShapeToKeyFrame shape $ updateShapes (M.insert ident shape) context

addGroup :: Ident -> [Ident] -> Context -> Context
addGroup ident idents context =
    case sequence $ map (flip M.lookup (views context)) idents of
      Nothing -> error $ "undefined view in: " ++ show idents
      Just vs -> updateGroups (M.insert ident (ident, vs)) context

setActive :: Ident -> Context -> Context
setActive ident context =
    case M.lookup ident (views context) of
      Just view -> context { activeViews = [view] }
      Nothing -> case M.lookup ident (groups context) of
                   Just (_, views) -> context { activeViews = views }
                   Nothing -> error $ "undefined view or group: " ++ show ident

writeShapeToKeyFrame :: Shape -> Context -> Context
writeShapeToKeyFrame shape context =
    let views = activeViews context
        f = writeShapeToViews shape views
    in updateState (updateFrameSets $ updateKeyFrame f) context

writeShapeToViews :: Shape -> [ViewT] -> Frame -> Frame
writeShapeToViews shape views frame =
    if null $ views \\ vs shape
    then foldr (writeShape shape) frame views
    else error $ "shape not defined on all views: " ++ show views

writeShape :: Shape -> ViewT -> Frame -> Frame
writeShape (Rect llx lly width height col _) (vname,_,_) frame =
    (DrawRect llx lly width height vname (colorName col)):frame
writeShape (Circ x y r col _) (vname,_,_) frame =
    (DrawCirc x y r vname (colorName col)):frame


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
                             vs = activeViews context
                             rect = Rect llx lly width height col vs
                        in ((), addShape ident rect context)
definition (Circle ident xExpr yExpr rExpr col) =
    Salsa $ \context -> let  x = eval context xExpr
                             y = eval context yExpr
                             r = eval context rExpr
                             vs = activeViews context
                             circle = Circ x y r col vs
                        in ((), addShape ident circle context)
definition (View ident) =
    Salsa $ \context -> ((), setActive ident context)
definition (Group ident idents) =
    Salsa $ \context -> ((), addGroup ident idents context)

defCom :: DefCom -> Salsa ()
defCom (Def def) = definition def
defCom (Com com) = liftC $ command com


runProg :: Integer -> Program -> Animation
runProg framerate program =
    let salsas = map defCom program
        ((), context) = runSalsa (sequence_ salsas) baseContext
    in animation context
