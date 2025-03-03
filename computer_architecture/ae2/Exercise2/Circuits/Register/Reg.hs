module Register.Reg where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational

-- reg1 is a register circuit with 1 bit of state.  It takes two
-- inputs: ld is a load control, and x is a data bit.  At a clock
-- tick, the register replaces its state with x if ld=1, but retains
-- its previous state if ld=0.  The register outputs its state
-- continuously.  This circuit is defined in the Registers library, so
-- normally you would import that library rather than including this
-- definition.

reg1 :: CBit a => a -> a -> a
reg1 ld x = r
  where r = dff (mux1 ld r x)

-- Normally reg1 would only output r, as q is just an internal signal.
-- However, it is enlightening for a student to experiment
-- interactively with the circuit, and to do that it's helpful to see
-- the value of q.

reg1q :: CBit a => a -> a -> (a,a)
reg1q ld x = (r, q)
  where r = dff q
        q = or2 (and2 (inv ld) r) (and2 ld x)

reg4 :: CBit a => a -> [a] -> ([a],[a])
reg4 ld [x3,x2,x1,x0] = ([r3,r2,r1,r0], [q3,q2,q1,q0])
  where r3 = dff q3
        r2 = dff q2
        r1 = dff q1
        r0 = dff q0
        q3 = or2 (and2 (inv ld) r3) (and2 ld x3)
        q2 = or2 (and2 (inv ld) r2) (and2 ld x2)
        q1 = or2 (and2 (inv ld) r1) (and2 ld x1)
        q0 = or2 (and2 (inv ld) r0) (and2 ld x0)
