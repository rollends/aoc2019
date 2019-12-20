{-# LANGUAGE NoImplicitPrelude #-}

module Day5
  ( day5
  ) where

import RIO
import qualified RIO.List as L
import qualified RIO.List.Partial as L'

import RIO.List.Partial((!!))

import Util

day5 :: Int -> [Int] ->Int
day5 input state =
  let
    initMemory = InitialState $ state
    initialComputer = Computer {memory = initMemory, pc = 0, input = input, output = []}
  in
    L'.head . L.reverse . output $ runComputerToTermination initialComputer

data ComputerMemory =
  InitialState [Int] |
  ModifiedState Int Int ComputerMemory

data Computer = Computer { memory :: ComputerMemory, pc :: Int, input :: Int, output :: [Int] }

data Instruction =
  Add Int Int Int | Multiply Int Int Int | Scan Int | Print Int | Stop

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

-- Reads a parameter for an instruction based on the mode
-- passed in (checks whether immediate or position mode)
readParameter :: ComputerMemory -> Int -> Int -> Int -> Int
readParameter state mode base offset =
  let
    read = readMemory state
    deref = read . read
  in
    if (mod (div mode (intPow 10 (offset-1))) 10) == 1 then
      read $ base + offset
    else
      deref $ base + offset

-- Reads an instruction from a given memory location and returns
-- the instruction as well as new PC
fetchInstruction :: Computer -> (Instruction, Int)
fetchInstruction comp =
  let
    state = memory comp
    loc = pc comp
    read = readMemory state
    (mode, opcode) = quotRem (read loc) 100
    param = readParameter state mode loc
    outParam = \x -> read $ loc + x
  in case opcode of
    1 -> (Add (param 1) (param 2) (outParam 3), loc + 4)
    2 -> (Multiply (param 1) (param 2) (outParam 3), loc + 4)
    3 -> (Scan (outParam 1), loc + 2)
    4 -> (Print (param 1), loc + 2)
    99 -> (Stop, loc)
    _ -> (Stop, loc)

-- Executes an Instruction to produce a new State as well as a flag to stop!
executeInstruction :: Computer -> (Instruction, Int) -> (Computer, Bool)

-- Add Instruction
executeInstruction comp (Add x y dest, newPC) =
  let
    state = memory comp
    readAt = readMemory state
    z = x + y
    newMemory = ModifiedState dest z state
  in
    (Computer{memory = newMemory, pc = newPC, input = input comp, output = output comp}, True)

-- Multiply Instruction
executeInstruction comp (Multiply x y dest, newPC) =
  let
    state = memory comp
    readAt = readMemory state
    z = x * y
    newMemory = ModifiedState dest z state
  in
    (Computer{memory = newMemory, pc = newPC, input = input comp, output = output comp}, True)

-- Scan Instruction
executeInstruction comp (Scan dest, newPC) =
  let
    state = memory comp
    newMemory = ModifiedState dest (input comp) state
  in
    (Computer{memory = newMemory, pc = newPC, input = input comp, output = output comp}, True)

-- Print Instruction
executeInstruction comp (Print value, newPC) =
  let
    state = memory comp
  in
    (Computer{memory = memory comp, pc = newPC, input = input comp, output = (output comp) ++ [value]}, True)

-- Stop Instruction. Return False to stop.
executeInstruction state (Stop,_) = (state, False)

-- Runs the Computer and returns the memory at termination
runComputerToTermination :: Computer -> Computer
runComputerToTermination computer =
  let
    fetch = fetchInstruction
    execute = executeInstruction computer
  in
    case execute . fetch $ computer of
      (finalState, False) -> finalState
      (newState, True) -> runComputerToTermination newState
