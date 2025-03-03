-- Sigma16 M1 circuit: ALU.hs
-- John T. O'Donnell, 2022
-- See Sigma16/README and https://jtod.github.io/home/Sigma16/

------------------------------------------------------------------------
--			Arithmetic/Logic Unit
------------------------------------------------------------------------

module M1.ALU where

-- Arithmetic and logic unit for the M1 processor circuit, which implements
-- the core subset of Sigma16.

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational

{- The ALU calculates a function of word inputs x and y (which are
usually the contents of two registers) and the condition code cc (the
contents of R15).

(The Core architecture doesn't need the cc as an input to the ALU, but
the more advanced instructions do, and cc is provided as an input to
simplify future zetension of M1.)

The ALU produces a word output r, which is a
numeric result (typically loaded into the destination register), and a
comparison result ccnew which is the new value to be loaded into the
condition code register.  The ALU performs addition, subtraction,
negation, increment, and comparision.  The function is determined by
control signals (alua, alub, aluc, alud).

Control inputs:
   alua, alub, aluc, alud
Data inputs:
   x
   y
   cc
Data outputs:
   r      = function (a,b,c,d) x y cc
   ccnew  = compare x y cc

The data output r is the result of an arithmetic operation which is
determinted by the control inputs:

| op = (alua,alub,aluc,alud)
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


The condition code flags are as follows.  Sigma16 indexes bits
starting from 0 in the rightmost (least significant) position, so the
flags cluster toward the right end of a word.  (Note: The Hydra !!
operator indexes bits from the left.)

| bit index | Relation        | Symbol |
|-----------+-----------------+--------|
|         0 | > Int           | >      |
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

and2w :: Bit a => [a] -> [a] -> [a]
and2w = map2 and2

or2w :: Bit a => [a] -> [a] -> [a]
or2w = map2 or2

xor2w :: Bit a => [a] -> [a] -> [a]
xor2w = map2 xor2

alu n (alua,alub,aluc,alud) x y cc = (result, ccnew)
  where

-- Constant words    
    wzero = fanout n zero
    wone = boolword n one

-- Determine type of function being evaluated
    arith = and2 (inv alua) (inv alub)  -- doing arithmetic operation, alu abc one of 0000 0001 0010 0011
    negating = and3 (inv alua) (inv alub) (xor2 aluc alud)  -- alu abc = 0001 or 0010
    logic = and2 alua (inv alub) -- doing logic operation, alu abc one of 1000 1001 1010 1011

-- Prepare inputs to adder    
    x'' = mux2w (aluc,alud) x x wzero x
    y'' = mux2w (aluc,alud) y (invw y) (invw x) wone
    x' = mux1w (alua) x'' x
    y' = mux1w (alua) y'' y

-- The adder    
    xy = bitslice2 x' y'
    (carry',result') = rippleAdd negating xy
    msb = result!!0 --- most significant bit of result

-- Binary comparison    
    (lt,eq,gt) = rippleCmp xy

-- Two's complement comparison    
    lt_tc = mux2 (xy!!0) lt zero one lt
    eq_tc = eq
    gt_tc = mux2 (xy!!0) gt one zero gt

-- Logic operations
    logicresult = mux2w (aluc, alud) (invw x') (and2w x' y') (or2w x' y') (xor2w x' y')
    trueresult = mux1w alua result' logicresult
    (carry, result) = (carry', trueresult)

-- Carry and overflow
    mx = x' !! 15      -- sign bit of first operand
    my = y' !! 15      -- sign bit of second operand
    mr = result !! 15  -- sign bit of result
    intovfl = or2 (and3 mx my (inv mr))
                  (and3 (inv mx) (inv my) mr)
    natovfl = carry
    noOvfl  = inv intovfl

-- Relation of integer result to 0
    any1 = orw result                  -- 1 if any bit in result is 1
    neg  = and3 noOvfl any1 msb        -- ok, result < 0
    z    = and2 noOvfl (inv any1)      -- ok, result is 0
    pos  = and3 noOvfl any1 (inv msb)  -- ok, result > 0

-- Overflow flags:  don't indicate overflow for a comparison operation
    fcarry   = and2 arith carry
    fnatovfl = and2 arith natovfl
    fintovfl = and2 arith intovfl

-- Comparison flags: for arithmetic, indicate comparison with 0
    flt      = mux1 arith lt    zero
    flt_tc   = mux1 arith lt_tc neg
    feq      = mux1 arith eq    z
    fgt      = mux1 arith gt    pos
    fgt_tc   = mux1 arith gt_tc pos

-- Generate the condition code
    ccnew = [ zero,   zero,     zero,     zero,    -- bit 15 14 13 12
              zero,   zero,     zero,     zero,    -- bit 11 10  9  8
              fcarry, fnatovfl, fintovfl, flt_tc,  -- bit  7  6  5  4
              flt,    feq,      fgt,      fgt_tc   -- bit  3  2  1  0
            ]
