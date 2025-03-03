-- Sigma16 M1: Datapath.hs
-- John T. O'Donnell, 2022
-- See Sigma16/README and https://jtod.github.io/home/Sigma16/

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module M1.Datapath where

-- This module defines the datapath for the M1 circuit, a processor
-- for the Sigma16 architecture.
 
-- The datapath contains the registers, computational systems, and
-- interconnections.  It has two inputs: a set of control signals
-- provided by the control unit, and a data word from the either the
-- memory system or the DMA input controller.

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational
import HDL.Hydra.Circuits.Register

import M1.Interface
import M1.ALU
import Register.RegFile
import Arithmetic.Multiply

datapath
  :: CBit a
  => CtlSig a
  -> SysIO a
  -> [a]
  -> DPoutputs a

datapath (CtlSig {..}) (SysIO {..}) memdat = dp
  where

-- Interface
    dp = DPoutputs {..}
    
-- Size parameters
    n = 16    -- word size

-- Registers
    (a,b,cc) = regFileSpec n            -- size parameter
                 ctl_rf_ld ctl_rf_ldcc  -- load controls
                 rf_d rf_sa rf_sb       -- register addresses
                 p ccnew                -- data inputs
    ir = reg n ctl_ir_ld memdat
    pc = reg n ctl_pc_ld q
    ad = reg n ctl_ad_ld u

-- ALU
    aluOutputs = alu n (ctl_alu_a, ctl_alu_b, ctl_alu_c, ctl_alu_d) x y cc
    (r,ccnew) = aluOutputs

-- Multiplier functional unit
    (mul_rdy, mul_prod32, mul_rx, mul_ry, mul_s)
      = multiply n ctl_mul_start a b
    mul_prod16 = field mul_prod32 16 16

-- Internal processor signals
    x = mux1w ctl_x_pc a pc             -- alu input 1
    y = mux1w ctl_y_ad b ad             -- alu input 2
    rf_d = mux1w ctl_rf_da ir_d ir_sa
    rf_sa = mux1w ctl_rf_sd ir_sa ir_d  -- a = reg[rf_sa]
    rf_sb = mux1w (and2 io_DMA io_regFetch)
              ir_sb
              (field io_address 12 4)
    p  = mux1w ctl_rf_pc                -- regfile data input
           (mux1w ctl_rf_prod
             (mux1w ctl_rf_alu memdat r)
             mul_prod16)
           pc

    q = mux1w ctl_pc_ad r ad        -- input to pc
    u = mux1w ctl_ad_alu memdat r   -- input to ad
    ma = mux1w ctl_ma_pc ad pc      -- memory address
    md = a                          -- memory data

-- Instruction fields
    ir_op = field ir  0 4           -- instruction opcode
    ir_d  = field ir  4 4           -- instruction destination register
    ir_sa = field ir  8 4           -- instruction source a register
    ir_sb = field ir 12 4           -- instruction source b register
