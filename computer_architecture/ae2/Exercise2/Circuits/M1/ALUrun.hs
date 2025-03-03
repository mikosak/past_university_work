-- Sigma16: ALUrun.hs
-- John T. O'Donnell, 2022
-- See Sigma16/README and https://jtod.github.io/home/Sigma16/

---------------------------------------------------------------------------
-- Simulation driver and test data for ALU
---------------------------------------------------------------------------

-- Usage: cd to the M1 directory and enter ghc -e main Circuit/ALUrun

module Main where
import HDL.Hydra.Core.Lib
import M1.ALU

{-

Result function

| a b c d |    r     |
|----------+-----------
| 0 0 0 0 |   x+y    | 
| 0 0 0 1 |   x-y    | 
| 0 0 1 0 |    -x    | 
| 0 0 1 1 |   x+1    | 
| 0 1 0 0 |   cmp    |
| 1 0 0 0 |    ~x    |
| 1 0 0 1 |   x&y    |
| 1 0 1 0 |   x|y    |
| 1 0 1 1 |   x^y    |

Condition code

| bit index | Relation        | Symbol |
|-----------+-----------------+--------|
|         0 | > Int           | g      |
|         1 | > Nat           | G      |
|         2 | =               | =      |
|         3 | < Nat           | L      |
|         4 | < Int           | <      |
|         5 | Int overflow    | v      |
|         6 | Nat overflow    | V      |
|         7 | Carry           | C      |
|         8 | Stack overflow  | S      |
|         9 | Stack underflow | s      |
-}

alu_input1 =
--   a  b  c  d      x      y   cc     Operation  Result
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  [ "0  0  0  0     14     15   0"    --   x+y       29
  , "0  0  0  0    125    590   0"    --   x+y      715
  , "0  0  0  0     49     15   0"    --   x+y       64
  , "0  0  0  0     21    -19   0"    --   x+y        2
  , "0  0  0  0     21    -35   0"    --   x+y      -14
  , "0  0  0  0   -350     75   0"    --   x+y     -275
  , "0  0  0  0   -420    -90   0"    --   x+y     -510

  , "0  0  0  1     49     15   0"    --   x-y       34
  , "0  0  0  1     15     49   0"    --   x-y      -34

  , "0  0  1  0     39      0   0"    --   -x       -39
  , "0  0  1  0     25     70   0"    --   -x       -25

  , "0  0  1  1     17      0   0"    --   x+1       18
  , "0  0  1  1    193     52   0"    --   x+1      194

  , "1  0  0  0    534      0   0"    --    ¬x     -535
  , "1  0  0  0     -9     10   0"    --    ¬x        8
  , "1  0  0  1     54     38   0"    --   x∧y       38
  , "1  0  0  1    543     26   0"    --   x∧y       26
  , "1  0  1  0     87   -100   0"    --   x∨y      -33
  , "1  0  1  0     21     65   0"    --   x∨y       85
  , "1  0  1  1   -100    427   0"    --   x⊕y     -457
  , "1  0  1  1    183      1   0"    --   x⊕y      182

  , "0  1  0  0      5      5   0"    --  cmp     cc = 0004  =
  , "0  1  0  0      5      7   0"    --  cmp     cc = 0018  <L
  , "0  1  0  0      7      5   0"    --  cmp     cc = 0003  >G
  , "0  1  0  0      5     -1   0"    --  cmp     cc = 0009  >L
  , "0  1  0  0     -1      5   0"    --  cmp     cc = 0012  <G

   ]


---------------------------------------------------------------------------
-- Simulation driver for ALU
---------------------------------------------------------------------------

main = driver  $ do

-- Word size
  let n =  16

-- Input data
  useData alu_input1

-- Input signals
  a <- inputBit "a"
  b <- inputBit "b"
  c <- inputBit "c"
  d <- inputBit "d"
  x <- inputWord "x" n
  y <- inputWord "y" n
  cc <- inputWord "cc" n

-- Circuit  
  let (r,ccnew) = alu n (a,b,c,d) x y cc

  format
      [string "Inputs:  ",
       string "abcd = ", bit a, bit b, bit c, bit d,
       string "\n         x = ", bits x, string " $", binhex x,
       string " (bin:", bindec 5 x, string ")",
       string " (tc: ", bitstc 6 x, string ")",
       string "\n         y = ", bits y, string " $", binhex y,
       string " (bin:", bindec 5 y, string ")",
       string " (tc: ", bitstc 6 y, string ")",
       string "\n       Outputs:  ",
       string "\n         r = ", bits r, tcdec 5 r, string "  $", binhex r,
       string "\n         ccnew = ", bits ccnew, string " $", binhex ccnew,
       string "\n"]
  runSimulation
