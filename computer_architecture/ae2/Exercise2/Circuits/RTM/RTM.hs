-- RTM: register transfer machine
-- This file is part of Hydra. See README and https://github.com/jtod/Hydra
-- Copyright (c) 2022 John T. O'Donnell

module RTM.RTM where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational

import Register.Reg
import Register.RegFile

-- import HDL.Hydra.Circuits.Register
-- import HDL.Hydra.Circuits.RegFile

-- Register transfer machine circuit

-- Size parameters
--   n = word size; each register contains n bits
--   k = file size; there are 2**k registers
-- The inputs are:
--   ld :: a (bit)
--      load control; 1 means load reg[d] := y
--   add :: a (bit)
--      data bus y control: y = if add=0 then x else s
--   d :: [a] (n-bit word)
--      destination register
--   sa :: [a] (k-bit word)
--      source register a (first operand)
--   sb :: [a] (k-bit word)
--      source register b (second operand)

rtm :: CBit a => Int -> Int
  -> a -> a -> [a] -> [a] -> [a] -> [a] -> ([a],[a],[a],a,[a])

rtm n k ld add d sa sb x = (a,b,y,c,s)
  where
    (a,b) = regfile n k ld d sa sb y
    y = mux1w add x s
    (c,s) = rippleAdd zero (bitslice2 a b)

