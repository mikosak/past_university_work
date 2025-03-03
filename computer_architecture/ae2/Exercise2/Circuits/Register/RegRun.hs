-- RegRun: simulate a word register circuit
-- This file is part of Hydra.  John O'Donnell, 2021.  See Hydra/README

-- This is an example of how to simulate a small circuit
-- interactively, where the circuit (myreg) uses words as well as
-- individual bits

module Register.RegRun where
import HDL.Hydra.Core.Lib   -- the hardware description language
import Register.Reg        -- definition of the circuit to be simulated

main :: IO ()
main = do
  putStrLn "Running reg4"
  run_reg4

  
run_reg4 :: IO ()
run_reg4 = driver $ do

-- Input ports
  ld <- inputBit "ld"
  x  <- inputWord "x" 4

-- Circuit defines output signals
  let (r,q) = reg4 ld x

  -- Output ports  
  outputWord "r" r
  outputWord "q" q

  -- Run simulation
  runSimulation
