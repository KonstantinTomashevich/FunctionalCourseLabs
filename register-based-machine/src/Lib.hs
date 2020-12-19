module Lib
    ( RegisterMachineState,
      runCommands,
      load,
      write,
      Lib.read,
      negt,
      sign,
      inc,
      dec,
      add,
      sub,
      mul,
      Lib.div,
      Lib.mod,
      fdiv
    ) where

import Control.Monad.State ( guard, MonadState(put, get), State, StateT(runStateT) )
import Data.Map ( Map, lookup, (!), insert )
import Control.Monad.Trans.Maybe ( MaybeT(runMaybeT) )
import Control.Monad.Identity ( Identity(runIdentity) )
import Data.Maybe ( isJust, fromJust )

type RegisterMachineState a b = MaybeT (State (Map Int a)) b

runCommands :: RegisterMachineState a b -> Map Int a -> Maybe a
runCommands commands registersState = do
  let result = runIdentity (runStateT (runMaybeT commands) registersState)
  case fst result of
    Nothing -> Nothing
    Just _ -> Just (snd result ! 0)

load :: a -> RegisterMachineState a ()
load newMainValue = do
    registersState <- get
    put (insert 0 newMainValue registersState)

write :: Int -> RegisterMachineState a ()
write targetIndex = do
    registersState <- get
    let mainValue = Data.Map.lookup 0 registersState
    guard $ isJust mainValue
    put (insert targetIndex (fromJust mainValue) registersState) 

read :: Int -> RegisterMachineState a ()
read targetIndex = do
    registersState <- get
    let valueAtIndex = Data.Map.lookup targetIndex registersState
    guard $ isJust valueAtIndex
    put (insert 0 (fromJust valueAtIndex) registersState)

unary :: (a -> a) -> RegisterMachineState a ()
unary operator = do 
    registersState <- get
    let mainValue = Data.Map.lookup 0 registersState
    guard $ isJust mainValue
    put (insert 0 (operator $ fromJust mainValue) registersState)

negt :: (Num a) => RegisterMachineState a ()
negt = unary negate

sign :: (Num a) => RegisterMachineState a ()
sign = unary signum

inc :: (Enum a) => RegisterMachineState a ()
inc = unary succ

dec :: (Enum a) => RegisterMachineState a ()
dec = unary pred

binary :: (a -> a -> Bool) -> (a -> a -> a) -> Int -> RegisterMachineState a ()
binary conditionChecker operator secondOperandIndex = do 
    registersState <- get
    let firstOperand = Data.Map.lookup 0 registersState
    guard $ isJust firstOperand
    let secondOperand = Data.Map.lookup secondOperandIndex registersState
    guard $ isJust secondOperand
    guard $ conditionChecker (fromJust firstOperand) (fromJust secondOperand)
    put (insert 0 (operator (fromJust firstOperand) (fromJust secondOperand)) registersState)

add :: (Num a) => Int -> RegisterMachineState a ()
add = binary (\_ _ -> True) (+)

sub :: (Num a) => Int -> RegisterMachineState a ()
sub = binary (\_ _ -> True) (-)

mul :: (Num a) => Int -> RegisterMachineState a ()
mul = binary (\_ _ -> True) (*)

div :: (Integral a) => Int -> RegisterMachineState a ()
div = binary (\_ y -> y /= 0) Prelude.div

mod :: (Integral a) => Int -> RegisterMachineState a ()
mod = binary (\_ y -> y /= 0) Prelude.mod

fdiv :: (Floating a) => Int -> RegisterMachineState a ()
fdiv = binary (\_ _ -> True) (/)