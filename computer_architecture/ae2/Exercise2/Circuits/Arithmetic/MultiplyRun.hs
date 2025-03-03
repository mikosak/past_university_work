-- MultiplyRun: simulation driver for multiply circuit
-- To run the multiplier: ghc -e main Circuits.Arithmetic.MultiplyRun

module Arithmetic.MultiplyRun where
import HDL.Hydra.Core.Lib
import Arithmetic.Multiply

-- On each clock cycle, the first two values on each line of the
-- simulation output are ready and result.  The rest of the values are
-- internal signals.  After a multiplication has started, look for the
-- first cycle where ready=1; on that line the value of result will be
-- the final product.  If the multiplier has not finished a
-- multiplication but start=1, the circuit will abandon the unfinished
-- multiplication and start a new one.

mult_test_data_1 :: [String]
mult_test_data_1 =
--     start  x    y
--     ~~~~~~~~~~~~~~
       ["1    7     9",  -- start=1 to multiply 7 * 9, expect 63
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "1    50   75",  -- start=1 to multiply 50 * 75, expect 3750
        "0    0     0",  -- start=0 to give circuit time
        "0    0     0",  -- a number of clock cycles are needed
        "0    0     0",  -- give it another cycle
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "1  100   100",  -- start=1 to multiply 100*100, expect 10000
        "0    0     0",  -- working on 100*100
        "0    0     0",  -- working on 100*100
        "1    2     3",  -- abort and start multiplying 2*3, expect 6
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0",
        "0    0     0"]

-- main :: Driver a
main :: IO ()
main = driver $ do

-- Input data  
  useData mult_test_data_1

-- Size parameter
  let k = 8

  start <- inputBit "start"
  x     <- inputWord "x" k
  y     <- inputWord "y" k

-- Circuit
  let (rdy,prod,rx,ry,s) = multiply k start x y

-- Format the signals
  format
    [string "Input: ",
     bit start, bindec 4 x, bindec 4 y,
     string "  Output: ",
     bit rdy, bindec 6 prod, bindec 4 rx, bindec 6 ry,
     bindec 6 s,
     string "\n"]

  runSimulation
