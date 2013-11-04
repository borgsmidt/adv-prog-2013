--
-- Skeleton for Salsa interpreter
-- To be used at the exam for Advanced Programming, B1-2013
--

module SalsaInterp
       -- (Position, interpolate, runProg)
where

import SalsaAst
import Gpx
import Data.List(union, (\\))
import qualified Data.Map as M

type Position = (Integer, Integer)
interpolate :: Integer -> Position -> Position -> [Position]
-- TODO
interpolate rate fromPos toPos = replicate (fromIntegral rate) toPos

data Shape = Rect {
      idnt :: Ident
    , width :: Integer
    , height :: Integer
    , col :: Colour
    , vs :: [ViewT]
    } | Circ {
      idnt :: Ident
    , r :: Integer
    , col :: Colour
    , vs :: [ViewT]
    } deriving (Show, Eq)

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
    }  deriving (Show, Eq)

type PosMap = M.Map Ident (M.Map ViewT Position)
type FrameSet = [Frame]
data State = State {
      shapePos :: PosMap
    , frameSets :: [FrameSet]
    } deriving (Show, Eq)

blankFrame = [] :: [GpxInstr]
-- TODO Clean this up
baseState = State M.empty [[blankFrame]]
baseContext rate = Context M.empty M.empty M.empty [] rate baseState

updateViews :: (ViewMap -> ViewMap) -> Context -> Context
updateViews f = \context -> context { views = f (views context) }

updateShapes :: (ShapeMap -> ShapeMap) -> Context -> Context
updateShapes f = \context -> context { shapes = f (shapes context) }

updateGroups :: (GroupMap -> GroupMap) -> Context -> Context
updateGroups f = \context -> context { groups = f (groups context) }

updateState :: (State -> State) -> Context -> Context
updateState f = \context -> context { state = f (state context) }

updateFrameSets :: ([FrameSet] -> [FrameSet]) -> State -> State
updateFrameSets f = \state -> state { frameSets = f (frameSets state) }

updateShapePos :: (PosMap -> PosMap) -> State -> State
updateShapePos f = \state -> state { shapePos = f (shapePos state) }

-- the key frame of a given context is always the latest frame in the latest frame set
-- and a context always has at least a key frame, so we can assume it is there when
-- pattern matching
updateKeyFrame :: (Frame -> Frame) -> [FrameSet] -> [FrameSet]
updateKeyFrame f = \((kf:set):sets) -> ((f kf):set):sets

activeShapes :: Context -> [Shape]
activeShapes context = let shps = M.elems $ shapes context
                           views = activeViews context
                       in filter (any (`elem` views) . vs) shps

animation :: Context -> Animation
animation context =
    let vs = M.elems $ views context
        fs = reverse . concat . frameSets $ state context
    in (vs , fs)

moveShapes :: [Ident] -> Pos -> Context -> State
moveShapes idents pos context =
    let moving = lookupIdents "moveShapes" context idents (shapes context)
        active = activeShapes context
        other = active \\ moving
        fs = if null $ moving \\ active
             then replicate (fromIntegral $ frameRate context) blankFrame
             else error $ "shapes not defined on all active views: " ++ show idents ++ "\ncontext: " ++ show other ++ show moving ++ show active
        fs' = foldr (writeShape context $ Just pos) fs moving
        fs'' = foldr (writeShape context Nothing) fs' other
        context' = foldr (updateActivePos $ Just pos) context moving
    in updateFrameSets (fs'' :) $ state context'

-- Writes a single shape, moving or stationary
writeShape :: Context -> Maybe Pos -> Shape -> FrameSet -> FrameSet
writeShape context pos shape fs =
    let fromPos = activePos shape context
        toPos = toPosition context fromPos pos
    in writeShapeViews shape (activeViews context) fromPos toPos fs

-- Writes a single shape on all active views across all frames in the frame set
writeShapeViews :: Shape -> [ViewT] -> [Position] -> [Position] -> FrameSet -> FrameSet
writeShapeViews shape views fromPos toPos fs =
    foldr (writeShapeViewFrames shape) fs (zip3 views fromPos toPos)

-- Writes a single shape on a single view across all frames in the frame set
writeShapeViewFrames :: Shape -> (ViewT, Position, Position) -> FrameSet -> FrameSet
writeShapeViewFrames shape (view, fromPos, toPos) fs =
    let pos = interpolate (toInteger $ length fs) fromPos toPos
    in zipWith (writeShapeViewFrame shape view) pos fs

-- Writes a single shape on a single view on a single frame
writeShapeViewFrame :: Shape -> ViewT -> Position -> Frame -> Frame
writeShapeViewFrame (Rect _ width height col _) (vname,_,_) (llx, lly) frame =
    (DrawRect llx lly width height vname (colorName col)):frame
writeShapeViewFrame (Circ _ r col _) (vname,_,_) (x, y) frame =
    (DrawCirc x y r vname (colorName col)):frame

activePos :: Shape -> Context -> [Position]
activePos shape context =
    let views = activeViews context
        pmap = lookupIdent "activePos" context (idnt shape) $ shapePos $ state context
    in lookupIdents "activePos" context views pmap

updateActivePos :: Maybe Pos -> Shape -> Context -> Context
updateActivePos pos shape context = 
    updateState (updateShapePos $ updatePosMap pos shape context) context

updatePosMap :: Maybe Pos -> Shape -> Context -> PosMap -> PosMap
updatePosMap pos shape context =
    \pmap -> let fromPos = activePos shape context
                 toPos = toPosition context fromPos pos
                 views = activeViews context
                 mp = lookupIdent "updatePosMap" context (idnt shape) pmap
                 mp' = foldl (\m (k, v) -> M.insert k v m) mp $ zip views toPos
             in M.insert (idnt shape) mp' pmap

addToPosMap :: Ident -> Position -> Context -> PosMap -> PosMap
addToPosMap ident pos context =
    \pmap -> let views = activeViews context
                 mp = foldl (\m k -> M.insert k pos m) M.empty views
             in M.insert ident mp pmap

toPosition :: Context -> [Position] -> Maybe Pos -> [Position]
toPosition _ fromPos Nothing = fromPos
toPosition context fromPos (Just (Abs xExpr yExpr)) =
    let x = eval context xExpr
        y = eval context yExpr
    in replicate (length fromPos) (x, y)
toPosition context fromPos (Just (Rel xExpr yExpr)) =
    let dx = eval context xExpr
        dy = eval context yExpr
    in map (\(x, y) -> (x+dx, y+dy)) fromPos

addView :: Ident -> ViewT -> Context -> Context
addView ident view context =
    activate ident $ updateViews (M.insert ident view) context

addShape :: Ident -> Shape -> Position -> Context -> Context
addShape ident shape pos context =
    let context' = updateShapes (M.insert ident shape) context
        context'' = updateState (updateShapePos $ addToPosMap ident pos context') context'
    in writeShapeToKeyFrame shape pos context''


writeShapeToKeyFrame :: Shape -> Position -> Context -> Context
writeShapeToKeyFrame shape pos context =
    let writer = \view frame -> writeShapeViewFrame shape view pos frame
        updater = \frame -> foldr writer frame $ activeViews context
    in updateState (updateFrameSets $ updateKeyFrame updater) context

addGroup :: Ident -> [Ident] -> Context -> Context
addGroup ident idents context =
    let vs = lookupIdents "addGroup" context idents (views context)
    in updateGroups (M.insert ident (ident, vs)) context

lookupIdent :: Ord b => String -> Context -> b -> M.Map b a -> a
lookupIdent msg context ident m =
    case M.lookup ident m of
      Nothing -> error $ "undefined object: " ++ "\ncontext: " ++ show context ++ msg
      Just v -> v

lookupIdents :: Ord b => String -> Context -> [b] -> M.Map b a -> [a]
lookupIdents msg context idents m = map (flip (lookupIdent msg context) m) idents

activate :: Ident -> Context -> Context
activate ident context =
{- I wonder if there is a clever way of stringing together multiple Maybes,
 - branching on Nothing. It is opposite of the usual Maybe Monad behavior where
 - the occurrence of a single Nothing forces the combined result to Nothing -}
    case M.lookup ident (views context) of
      Just view -> context { activeViews = [view] }
      Nothing -> case M.lookup ident (groups context) of
                   Just (_, views) -> context { activeViews = views }
                   Nothing -> error $ "undefined view or group: " ++ show ident


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


-- Changes the context locally for the command
local :: (Context -> Context) -> SalsaCommand a -> SalsaCommand a
local f m = SalsaCommand $ \context -> runSC m (f context)

command :: Command -> SalsaCommand ()
command (At com ident) = local (activate ident) $ command com
command (Par com0 com1) = command com0 >> command com1 >> mergeConcurrent ()
command (Move idents pos) =
    SalsaCommand $ \context -> let state = moveShapes idents pos context
                               in ((), state)


mergeConcurrent :: a -> SalsaCommand a
mergeConcurrent x =
    SalsaCommand $ \context -> (x, updateFrameSets mergeTwoFrameSets $ state context)

mergeTwoFrameSets :: [FrameSet] -> [FrameSet]
mergeTwoFrameSets (fs0:fs1:sets) = (zipWith union fs0 fs1):sets


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
eval _ (Const val) = val
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
                             rect = Rect ident width height col vs
                        in ((), addShape ident rect (llx, lly) context)
definition (Circle ident xExpr yExpr rExpr col) =
    Salsa $ \context -> let  x = eval context xExpr
                             y = eval context yExpr
                             r = eval context rExpr
                             vs = activeViews context
                             circle = Circ ident r col vs
                        in ((), addShape ident circle (x, y) context)
definition (View ident) =
    Salsa $ \context -> ((), activate ident context)
definition (Group ident idents) =
    Salsa $ \context -> ((), addGroup ident idents context)

defCom :: DefCom -> Salsa ()
defCom (Def def) = definition def
defCom (Com com) = liftC $ command com


runProg :: Integer -> Program -> Animation
runProg framerate program =
    let salsas = map defCom program
        ((), context) = runSalsa (sequence_ salsas) $ baseContext framerate
    in animation context
