--
-- Skeleton for Salsa interpreter
-- To be used at the exam for Advanced Programming, B1-2013
--

module SalsaInterp
       -- (Position, interpolate, runProg)
where

import SalsaAst
import Gpx

--
-- The function interpolate
--

type Position = (Integer, Integer)
interpolate :: Integer -> Position -> Position -> [Position]

--
-- Define the types Context and SalsaCommand
--

data Context = ...

newtype SalsaCommand a = SalsaCommand {runSC :: Context -> ... }


-- functions for manipulating the context



--
-- Define the function command
--

command :: Command -> SalsaCommand ()


--
-- Define the type Salsa
--

data Salsa a = ...


--
-- Define the functions liftC, definition, and defCom
--

liftC :: SalsaCommand a -> Salsa a

definition :: Definition -> Salsa ()

defCom :: DefCom -> Salsa ()


--
-- Define the function runProg
--

runProg :: Integer -> Program -> Animation
