-- Reg1RunInteractive: simulate a 1-bit register circuit
-- Copyright (C) 2021 John T. O'Donnell.  This file is part of Sigma16.
-- See Sigma16/README and https://jtod.github.io/home/Sigma16/

-- This is an example of how to simulate a small circuit
-- interactively.  The circuit (myreg1) uses individual bits.  For an
-- example that also uses words, see RegRun.  See the User Guide and
-- Sigma16/src/circuits/README.

module Reg1RunInteractive where
import HDL.Hydra.Core.Lib   -- the hardware description language
import Register.Reg        -- definition of the circuit to be simulated

main :: IO ()
main = driver $ do

-- Input ports
  ld <- inputBit "ld"
  x  <- inputBit "x"

-- The circuit myreg1 receives input signals and defines output signals
  let (r,q) = reg1q ld x

-- The output ports are interfaces to the output signals
  outputBit "r" r
  outputBit "q" q

-- Run interactive simulation 
  runSimulation
