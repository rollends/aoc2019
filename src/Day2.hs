{-# LANGUAGE NoImplicitPrelude #-}

module Day2
  ( day2, day2part2
  ) where

import RIO
import qualified RIO.List as L
import qualified RIO.List.Partial as L'

import RIO.List.Partial((!!))

import Util

day2 :: Int -> Int -> [Int] ->Int
day2 a b state=
  let
    initMemory = (ModifiedState 2 b) . (ModifiedState 1 a) . InitialState $ state
    initialComputer = Computer {memory = initMemory, pc = 0}
  in
    readMemory (runComputerToTermination initialComputer) 0

data ComputerMemory =
  InitialState [Int] |
  ModifiedState Int Int ComputerMemory

data Computer = Computer { memory :: ComputerMemory, pc :: Int }

data Instruction =
  Add Int Int Int | Multiply Int Int Int | Stop

-- Reads value at address of computer's memory.
-- This is a recursive call that starts at the latest
-- modification and works backwards
readMemory :: ComputerMemory -> Int -> Int

-- At the first state, the value of memory is exactly
-- what is found in that address of the memory.
readMemory (InitialState a) loc =
  a !! loc

-- If the memory was modified at the location we are
-- reading, return the modified value, otherwise go
-- farther back in time to find modifications.
readMemory (ModifiedState mloc mval prevState) loc =
  if mloc == loc then
    mval
  else
    readMemory prevState loc

-- Reads an instruction from a given memory location.
-- Only reads an extra 3 arguments if necessary.
fetchInstruction :: ComputerMemory -> Int -> Instruction
fetchInstruction state loc =
  case (readMemory state loc) of
    1 -> uncurry3 Add $ fetchArguments state (loc + 1)
    2 -> uncurry3 Multiply $ fetchArguments state (loc + 1)
    99-> Stop

-- Simply reads 3 integers in-order from memory.
fetchArguments :: ComputerMemory -> Int -> (Int, Int, Int)
fetchArguments state loc =
  let
    readAt = readMemory state
  in
    (readAt $ loc, readAt $ loc + 1, readAt $ loc + 2)

-- Executes an Instruction to produce a new State as well as a flag to stop!
executeInstruction :: ComputerMemory -> Instruction -> (ComputerMemory, Bool)

-- Add Instruction
executeInstruction state (Add srcX srcY dest) =
  let
    readAt = readMemory state
    x = readAt srcX
    y = readAt srcY
    z = x + y
  in
    (ModifiedState dest z state, True)

-- Add Instruction
executeInstruction state (Multiply srcX srcY dest) =
  let
    readAt = readMemory state
    x = readAt srcX
    y = readAt srcY
    z = x * y
  in
    (ModifiedState dest z state, True)

-- Stop Instruction. Return False to stop.
executeInstruction state Stop = (state, False)


-- Runs the Computer and returns the memory at termination
runComputerToTermination :: Computer -> ComputerMemory
runComputerToTermination computer =
  let
    mem = memory computer
    fetch = (fetchInstruction mem) . (pc)
    execute = executeInstruction mem
  in
    case execute (fetch computer) of
      (finalState, False) -> finalState
      (newState, True) -> runComputerToTermination (Computer {memory = newState, pc = 4 + pc computer})

-- Find input that gives output
day2part2 :: Int -> [Int] -> Int

day2part2 number state = day2part2_internal state number 0

--Failed to find an input that works...
day2part2_internal _ _ 10000 = -1

day2part2_internal state numberToFind i =
  let
    a = div i 100
    b = mod i 100
  in
    if numberToFind == day2 a b state then
      100 * a + b
    else
      day2part2_internal state numberToFind (i+1)

