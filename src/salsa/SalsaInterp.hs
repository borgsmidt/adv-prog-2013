--
-- Skeleton for Salsa interpreter
-- To be used at the exam for Advanced Programming, B1-2013
--

module SalsaInterp (Position, interpolate, runProg)
where

import SalsaAst
import Gpx
import Data.List(intersect, union, (\\))
import qualified Data.Map as M

type Position = (Integer, Integer)


--
-- Primary top-level function
--

runProg :: Integer -> Program -> Animation
runProg framerate program =
    let salsas = map defCom program
        ((), context) = runSalsa (sequence_ salsas) $ baseContext framerate
    in animation context

--
-- The function interpolate
--

interpolate :: Integer -> Position -> Position -> [Position]
interpolate rate (x0, y0) (x1, y1)
    | rate <= 0 = error "framerate must be positive"
    | otherwise = zip (ipol rate x0 x1) (ipol rate y0 y1)

ipol :: Integer -> Integer -> Integer -> [Integer]
ipol rate start end =
    let step = (fromIntegral $ end - start) / fromIntegral rate
    in map ((start +) . round . (* step)) ([1.0..fromIntegral rate] :: [Double])

--
-- Data type representing the two types of shape
--
-- A shape has an identifier, some shape-specific information, a color name
-- and a list of the views it is defined on
data Shape = Rect Ident Integer Integer String [ViewT]
           | Circ Ident Integer String [ViewT]
  deriving (Show, Eq, Ord)

idnt :: Shape -> Ident
idnt (Rect ident _ _ _ _) = ident
idnt (Circ ident _ _ _) = ident

vs :: Shape -> [ViewT]
vs (Rect _ _ _ _ vws) = vws
vs (Circ _ _ _ vws) = vws

--
-- Data types relating to the (by commands) read-only environment and writable state
--

-- 'T' suffix is just to distinguish from Salsa AST constructors
type ViewT = (Ident, Integer, Integer)
type GroupT = (Ident, [ViewT])
type ViewMap = M.Map Ident ViewT
type ShapeMap = M.Map Ident Shape
type GroupMap = M.Map Ident GroupT
-- The Context represents all the information, both read-only and writable
data Context = Context {
      views :: ViewMap
    , shapes :: ShapeMap
    , groups :: GroupMap
    , activeViews :: [ViewT]
    , frameRate :: Integer
    , state :: State
    }  deriving (Show, Eq)

type PosMap = M.Map Ident (M.Map ViewT Position)
-- An ExtFrame associates each graphics command with the shape it draws
-- This information is used to combine frame sets produced by commands that need
-- to run in parallel
type ExtFrame = [(Ident, GpxInstr)]
type FrameSet = [ExtFrame]
-- The State data type is used for the writable information
-- shapePos is a map of maps, storing the current position of each shape on each view
-- A FrameSet stores 'framerate' number of frames, and each frame set holds the
-- frames between one key frame (excl) and the next (incl)
-- The frames of a frame set are stored in reverse order, so the next key frame
-- of a frame set is always at the head of the list
-- The frame sets are also held in reverse order with the most recent at the head
data State = State {
      shapePos :: PosMap
    , frameSets :: [FrameSet]
    } deriving (Show, Eq)

blankFrame :: ExtFrame
blankFrame = []

-- The base state has a single frame in a single frame set representing the single
-- initial key frame
baseState :: State
baseState = State M.empty [[blankFrame]]

-- The base context holds empty maps, the framerate and a base state
baseContext :: Integer -> Context
baseContext rate = Context M.empty M.empty M.empty [] rate baseState

--
-- functions to query and manipulate the context
--

updateViews :: (ViewMap -> ViewMap) -> Context -> Context
updateViews f = \context -> context { views = f (views context) }

updateShapes :: (ShapeMap -> ShapeMap) -> Context -> Context
updateShapes f = \context -> context { shapes = f (shapes context) }

updateGroups :: (GroupMap -> GroupMap) -> Context -> Context
updateGroups f = \context -> context { groups = f (groups context) }

updateState :: (State -> State) -> Context -> Context
updateState f = \context -> context { state = f (state context) }

updateFrameSets :: ([FrameSet] -> [FrameSet]) -> State -> State
updateFrameSets f = \st -> st { frameSets = f (frameSets st) }

updateShapePos :: (PosMap -> PosMap) -> State -> State
updateShapePos f = \st -> st { shapePos = f (shapePos st) }

-- the key frame of a given context is always the latest frame in the latest frame set
-- and a context always has at least a key frame
updateKeyFrame :: (ExtFrame -> ExtFrame) -> [FrameSet] -> [FrameSet]
updateKeyFrame f = \((kf:set):sets) -> ((f kf):set):sets

-- Returns the shapes present on the active views
activeShapes :: Context -> [Shape]
activeShapes context = let shps = M.elems $ shapes context
                           vws = activeViews context
                       in filter (any (`elem` vws) . vs) shps

-- Helper function that determines which shapes need to move
shapesToMove :: [Ident] -> Context -> [Shape]
shapesToMove idents context =
    let moving = lookupObjs "moveShapes" context idents (shapes context)
        active = activeShapes context
    in if null $ moving \\ active
       then moving
       else error $ "shapes not defined on all active views: " ++ show idents
                ++ "\ncontext: " ++ show context

-- Helper function that returns the position of a given shape on a given view
shapeViewPos :: Shape -> ViewT -> Context -> Position
shapeViewPos shape view context =
    let posMap = shapePos $ state context
        pmap = lookupObj "" context (idnt shape) posMap
    in lookupObj "" context view pmap

activePos :: Shape -> Context -> [Position]
activePos shape context =
    let vws = activeViews context
        pmap = lookupObj "activePos" context (idnt shape) $ shapePos $ state context
    in lookupObjs "activePos" context vws pmap

updateActivePos :: Maybe Pos -> Shape -> Context -> Context
updateActivePos pos shape context = 
    updateState (updateShapePos $ updatePosMap pos shape context) context

updatePosMap :: Maybe Pos -> Shape -> Context -> PosMap -> PosMap
updatePosMap pos shape context =
    \pmap -> let fromPos = activePos shape context
                 toPos = getToPositions context fromPos pos
                 vws = activeViews context
                 mp = lookupObj "updatePosMap" context (idnt shape) pmap
                 mp' = foldl (\m (k, v) -> M.insert k v m) mp $ zip vws toPos
             in M.insert (idnt shape) mp' pmap

addToPosMap :: Ident -> Position -> Context -> PosMap -> PosMap
addToPosMap ident pos context =
    \pmap -> let vws = activeViews context
                 mp = foldl (\m k -> M.insert k pos m) M.empty vws
             in M.insert ident mp pmap

-- Returns the Animation produced by this interpreter
-- This just flattens the list of frames and reverses it
animation :: Context -> Animation
animation context =
    let vws = M.elems $ views context
        fs = reverse . concat . frameSets $ state context
    in (vws , map stripFrame fs)

-- Removes the additional shape information from each frame to produce frames
-- suitable for the graphics backend
stripFrame :: ExtFrame -> Frame
stripFrame pairs = snd $ unzip pairs


--
-- Functions generating graphics instructions for moving shapes
--

-- Top-level function that causes the generation of the necessary graphics
-- instructions to move a set of shapes to a new absolute or relative position.
-- This function is responsible for generating each new frame set.
-- Strategy: Generate a complete set of instructions to draw everything in place;
-- then overwrite the instructions for the shapes that needs to move
moveShapes :: [Ident] -> Pos -> Context -> State
moveShapes idents pos context =
    let moving = shapesToMove idents context
        -- 'neutral' map with full set of combinations to draw everything in its
        -- current position
        wmap = M.fromList [((s, v), (shapeViewPos s v context, Nothing)) |
                            s <- M.elems (shapes context), v <- vs s]
        -- list of shapes to move, used to overwrite entries in the map
        moves = [((s, v), (shapeViewPos s v context, Just pos)) |
                  s <- moving, v <- activeViews context]
        wmap' = foldl (\m (k, v) -> M.insert k v m) wmap moves
        fs = replicate (fromIntegral $ frameRate context) blankFrame
        fs' = foldl (writeToFrameSet context) fs $ M.toList wmap'
        context' = foldr (updateActivePos $ Just pos) context moving
    in updateFrameSets (fs' :) $ state context'

-- Helper function that updates a frame set with instructions for a
-- particular shape and view
writeToFrameSet :: Context -> FrameSet -> ((Shape, ViewT), (Position, Maybe Pos))
                -> FrameSet
writeToFrameSet context fs ((shape, view), (fromPos, p)) =
    let toPos = getToPos context fromPos p
        -- the frames in a frame set is stored in reverse order so the key frame
        -- is at the front of the list
        pos = reverse $ interpolate (toInteger $ length fs) fromPos toPos
    in zipWith (writeToFrame shape view) pos fs
    
-- Helper function that writes a single shape on a single view to a single frame
writeToFrame :: Shape -> ViewT -> Position -> ExtFrame -> ExtFrame
writeToFrame (Rect ident width height colname _) (vname,_,_) (llx, lly) frame =
    (ident, (DrawRect llx lly width height vname colname)):frame
writeToFrame (Circ ident r colname _) (vname,_,_) (x, y) frame =
    (ident, (DrawCirc x y r vname colname)):frame

-- Helper functions that determine new positions based on current position
-- and absolute/relative position information
getToPositions :: Context -> [Position] -> Maybe Pos -> [Position]
getToPositions context positions pos = map (\p -> getToPos context p pos) positions

getToPos :: Context -> Position -> Maybe Pos -> Position
getToPos _ fromPos Nothing = fromPos
getToPos context _ (Just (Abs xExpr yExpr)) =
    (eval context xExpr, eval context yExpr)
getToPos context (x, y) (Just (Rel xExpr yExpr)) =
    (x + eval context xExpr, y + eval context yExpr)

--
-- Functions for implementing new definitions in context and current frame
--

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
    let writer = \view frame -> writeToFrame shape view pos frame
        updater = \frame -> foldr writer frame $ activeViews context
    in updateState (updateFrameSets $ updateKeyFrame updater) context

addGroup :: Ident -> [Ident] -> Context -> Context
addGroup ident idents context =
    let vws = lookupObjs "addGroup" context idents (views context)
    in updateGroups (M.insert ident (ident, vws)) context

activate :: Ident -> Context -> Context
activate ident context =
{- I wonder if there is a clever way of stringing together multiple Maybes,
 - branching on Nothing. It is opposite of the usual Maybe Monad behavior where
 - the occurrence of a single Nothing forces the combined result to Nothing -}
    case M.lookup ident (views context) of
      Just view -> context { activeViews = [view] }
      Nothing -> case M.lookup ident (groups context) of
                   Just (_, vws) -> context { activeViews = vws }
                   Nothing -> error $ "undefined view or group: " ++ show ident


--
-- Monad types SalsaCommand and Salsa
--

-- This type reflects that running a command cannot update the environment,
-- just the state
-- The type captures the effect of a command in a given context, that is a move
-- from the current key frame to a new
newtype SalsaCommand a = SalsaCommand { runSC :: Context -> (a, State) }
instance Monad SalsaCommand where
    return x = SalsaCommand $ \context -> (x, state context)
    m >>= f = SalsaCommand $ \context -> let (x, st) = runSC m context
                                         in runSC (f x) context { state = st }

-- The Salsa type represents an animation step; it is either a definition
-- activating something or adding a new shape to views on the current key frame,
-- or it is a new command that causes the generation of a new frame set from one
-- key frame to the next
newtype Salsa a = Salsa { runSalsa :: Context -> (a, Context) }
instance Monad Salsa where
    return x = Salsa $ \context -> (x, context)
    m >>= f = Salsa $ \context -> let (x, context') = runSalsa m context
                                  in runSalsa (f x) context'

-- Changes the context locally for the command
local :: (Context -> Context) -> SalsaCommand a -> SalsaCommand a
local f m = SalsaCommand $ \context -> runSC m (f context)

-- Captures the effect of a command in a SalsaCommand
command :: Command -> SalsaCommand ()
command (At com ident) = local (activate ident) $ command com
command (Par com0 com1) = command com0 >> command com1 >> mergeConcurrent com0 com1 ()
command (Move idents pos) =
    SalsaCommand $ \context -> ((), moveShapes idents pos context)

-- Helper function that captures the effect of running to commands in parallel
mergeConcurrent :: Command -> Command -> a -> SalsaCommand a
mergeConcurrent com0 com1 x =
    SalsaCommand $ \context ->
        (x, updateFrameSets (mergeFrameSets com0 com1) $ state context)

-- Helper function that merges the two latest frame sets. This is necessary when
-- commands should run in parallel and therefore manipulate the same frame set
-- Strategy: take the latest frame set (head of list), which was generated by
-- com1, and copy across any instruction pertaining to shapes manipulated by com0.
mergeFrameSets :: Command -> Command -> [FrameSet] -> [FrameSet]
mergeFrameSets com0 com1 (fs1:fs0:sets) =
    let shps0 = shapesFromCommand com0
        shps1 = shapesFromCommand com1
    in if null $ intersect shps0 shps1
       then (zipWith (mergeFrames shps1 shps0) fs1 fs0):sets
       else error $ "concurrent commands manipulating same shapes: "
                ++ show shps0 ++ " and " ++ show shps1
mergeFrameSets _ _ _ = error "invalid frame set configuration"

-- Strategy: In order not to overwrite the wrong instructions, we take the two
-- frame sets and remove from each, any instruction pertaining to a shape
-- manipulated ny the other command. After this, it is safe just to combine the
-- frame sets with union
mergeFrames :: [Ident] -> [Ident] -> ExtFrame -> ExtFrame -> ExtFrame
mergeFrames shps1 shps0 frame1 frame0 =
    let frame0' = filter (\(ident, _) -> not $ ident `elem` shps1) frame0
        frame1' = filter (\(ident, _) -> not $ ident `elem` shps0) frame1
    in union frame1' frame0'

-- Helper function that determines the shapes manipulated by a given command.
-- This is used when merging frame sets for parallel commands
shapesFromCommand :: Command -> [Ident]
shapesFromCommand (At com _) = shapesFromCommand com
shapesFromCommand (Par com0 com1) = shapesFromCommand com0 ++ shapesFromCommand com1
shapesFromCommand (Move idents _) = idents

-- Recursive evaluation function for the Salsa Expression type
eval :: Context -> Expr -> Integer
eval _ (Const val) = val
eval context (Plus expr0 expr1) = eval context expr0 + eval context expr1
eval context (Minus expr0 expr1) = eval context expr0 - eval context expr1
eval context (Xproj ident) =
    let shape = lookupObj "eval Xproj" context ident (shapes context)
        (xpos,_) = unzip $ activePos shape context
    in foldl min 0 xpos
eval context (Yproj ident) =
    let shape = lookupObj "eval Yproj" context ident (shapes context)
        (_,ypos) = unzip $ activePos shape context
    in foldl min 0 ypos

-- Captures the effect of a definition in a Salsa computation
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
                             vws = activeViews context
                             rect = Rect ident width height (colorName col) vws
                        in ((), addShape ident rect (llx, lly) context)
definition (Circle ident xExpr yExpr rExpr col) =
    Salsa $ \context -> let  x = eval context xExpr
                             y = eval context yExpr
                             r = eval context rExpr
                             vws = activeViews context
                             circle = Circ ident r (colorName col) vws
                        in ((), addShape ident circle (x, y) context)
definition (View ident) =
    Salsa $ \context -> ((), activate ident context)
definition (Group ident idents) =
    Salsa $ \context -> ((), addGroup ident idents context)

-- Helper function to wrap a SalsaCommand as a Salsa computation
liftC :: SalsaCommand a -> Salsa a
liftC sc = Salsa $ \context -> let (x, st) = runSC sc context
                               in (x, context { state = st })

-- Helper function to generate a Salsa computation from a DefCom
defCom :: DefCom -> Salsa ()
defCom (Def def) = definition def
defCom (Com com) = liftC $ command com

--- Other helper functions

-- looks up objects in a map and throws an error if the element(s) are not there.
lookupObjs :: Ord b => String -> Context -> [b] -> M.Map b a -> [a]
lookupObjs msg context idents m = map (flip (lookupObj msg context) m) idents

lookupObj :: Ord b => String -> Context -> b -> M.Map b a -> a
lookupObj msg context ident m =
    case M.lookup ident m of
      Nothing -> error $ "undefined object: " ++ "\ncontext: " ++ show context ++ msg
      Just v -> v

colorName :: Colour -> String
colorName Blue = "blue"
colorName Plum = "plum"
colorName Red = "red"
colorName Green = "green"
colorName Orange = "orange"
