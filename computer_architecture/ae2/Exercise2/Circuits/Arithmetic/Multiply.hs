module Arithmetic.Multiply where
-- Binary multiplier circuit

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational
import HDL.Hydra.Circuits.Register

multiply
  :: CBit a               -- synchronous circuit
  => Int                  -- k = input word size; output is 2*k
  -> a                    -- start = control input
  -> [a]                  -- x = k-bit word
  -> [a]                  -- y = k-bit word
  -> (a,[a],[a],[a],[a])  -- (ready, product, ...internalsignals...)

multiply k start x y = (ready,prod,rx,ry,s)
  where
    rx = latch k (mux1w start (shr rx) x)
    ry = latch (2*k)
             (mux1w start
                (shl ry)
                (fanout k zero ++ y))
    prod = latch (2*k)
             (mux1w start
                (mux1w (lsb rx) prod s)
                (fanout (2*k) zero))
    (c,s) = rippleAdd zero (bitslice2 ry prod)
    ready = or2 (inv (orw rx)) (inv (orw ry))
