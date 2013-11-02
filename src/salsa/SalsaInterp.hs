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

--
-- The function interpolate
--

type Position = (Integer, Integer)
interpolate :: Integer -> Position -> Position -> [Position]

--
-- Define the types Context and SalsaCommand
--

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
      groups :: [GroupDef]
      shapes :: M.Map Ident Shape
    , definitions :: M.Map Ident Definition
    , activeViews :: [Ident]
    , frameRate :: Integer
    }

data State = State {
      positions :: M.Map (Ident, Ident) Pos
      frames :: [Frame]
    }

data Context = Context {
      environment :: Environment
    , state :: State
    }

-- functions for manipulating the context

-- Changes the context locally for the command
local :: (Context -> Context) -> SalsaCommand a -> SalsaCommand a
local f m = SC $ \context -> runSC m (f context)

animation :: Context -> Animation
animation (Context env state) = (viewDefs env, frames state)

-- When talking about this type, point out that no error handling is necessary
-- This type reflects that running a command cannot update the environment,
-- just the state
-- The type captures the effect of a command in a given context
newtype SalsaCommand a = SalsaCommand { runSC :: Context -> (a, State) }
instance Monad SalsaCommand where
    return x = SalsaCommand $ \context -> (x, state context)
{- NOTE: Two sequenced SalsaCommands are deliberately executed in the same context,
 - the state is not updated before the second execution -}
    m >>= f = SalsaCommand $ \context -> let (x, st) = runSC m context
                                         in runSC (f x) context

newtype Salsa a = Salsa { runSalsa :: Context -> (a, Context) }
instance Monad Salsa where
    return x = Salsa $ \context -> (x, context)
    m >>= f = Salsa $ \context -> let (x, context') = runSalsa m context
                                  in runSalsa (f x) context'



--
-- Define the function command
--

command :: Command -> SalsaCommand ()
command (At com ident) = local (setActive ident) $ command com
command (Par com0 com1) = local clearFrames $ do ((), st0) <- command com0
                                                 ((), st1) <- command com1
                                                 setFrames $ zipWith (++) st0 st1
command (Move idents pos) =
    SalsaCommand $ \context -> let current = 


 env = environment context
                                   st = state context
                                   views = activeViews env



 updateAndManipulateState context




newState = state $ foldl (moveShape pos) context idents
                              


data Command = Move [Ident] Pos
             | At Command Ident
             | Par Command Command





--
-- Define the functions liftC, definition, and defCom
--

liftC :: SalsaCommand a -> Salsa a
liftC scom = SalsaCommand $ \context -> let (x, st) <- runSC scom context
                                        in (x, context { state = st }
                


definition :: Definition -> Salsa ()

defCom :: DefCom -> Salsa ()
defCom (Def def) = definition def
defCom (Com com) = liftC $ command com


runProg :: Integer -> Program -> Animation
runProg framerate program =
    let salsas = map defCom program
        ((), context) = apply (sequence_ salsas) emptyContext
    in animation context
