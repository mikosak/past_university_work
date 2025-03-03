---------------------------------------------------------------------------
-- M1run.hs: Simulation driver for M1 processor circuit
-- Copyright (c) 2023 John T. O'Donnell
---------------------------------------------------------------------------

-- Quick start:
--   $ cd cscode/circuits
--   $ ghc -e main M1/M1run
--   M1> boot Simple/Add
--   M1> run
--   M1> regs
--   M1> mem 0 10
--   M1> quit

-- M1 is a digital circuit that implements a processor for the Sigma16
-- architecture.  This module is a simulation driver: it defines an
-- interactive command loop that provides user commands to control the
-- simulation of the M1 circuit.  It implements breakpoints,
-- input/output, and facilities for displaying registers and memory.
-- The cscode/circuits/README file documents how to use the driver.

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module M1run where

import HDL.Hydra
import HDL.Hydra.Core.Lib   -- Hydra hardware description language
import Utility.ReadSigma16Obj      -- Read and parse Sigma16 object code file
import M1.System     -- The M1 processor circuit

import System.Environment
import System.IO
import Control.Monad.State
import Control.Exception
import qualified Data.Map as Map
import Data.Char

---------------------------------------------------------------------------
-- Configuration
---------------------------------------------------------------------------

-- If M1/progdir.txt exists, then the booter will look there for the
-- object code.  Otherwise it will look in defaultProgramPath

defaultProgramPath :: String
defaultProgramPath = "../Programs/Sigma16/Core/"

---------------------------------------------------------------------------
-- Clock cycle
---------------------------------------------------------------------------

-- Structure of operation that takes one clock cycle

autoPrintPorts :: StateT (SysState b) IO ()
autoPrintPorts = do
    printInPorts
    printOutPorts


---------------------------------------------------------------------------
-- Structure of the M1 driver
---------------------------------------------------------------------------

{-
An interactive command could request something unusual (such as a
register dump).  However, usually the user will wish to
establishM1inputs to the next cycle using the normal inputs.  This
is performed by establishM1inputs: perform the next action if the
current input list is not exhausted; otherwise go to the next mode

If an input list is being consumed, establishM1inputs continues as
long as there is data.  When the input list is exhaused, it goes to
the next normal mode.

DMA is a special case.  A DMA operation may take a number of clock
cycles, and it interrupts normal execution of the processor.  After
the DMA is finished, the previous mode is resumed.  When a DMA
begins, the processor mode is saved in stolenMode.  A DMA operation
will always run to completion; it cannot be interrupted by yet
another DMA.

-- M1 clock cycle

Every clock cycle consists of a sequence of phases:

- *choose* -- place inputs for current cycle into (currentInputString
  field of SysState).  Decide what inputs to use during the upcoming
  cycle, before it starts, and save the inputs as a string in a
  canonical format; it is placed in the currentInputString field of
  SysState.  The choice may depend on the driver state, annd/or it may
  depend on some input provided interactively by the user.  Once the
  inputs are chosen, all the signal values are fixed for the cycle
  (although what is actually output can be chosen later)

  - establish inputs - th
  - read the signals during the cycle
  - advance

The only thing that affects what happens during the cycle is the
inputs; controlling the cycle is done by establish inputs.  The middle
section just determines what is output, but not any signal values.
The advance has no choices to make; this is purely mechanical.

I/O control inputs (defined in System module):
  dma         1 bit    indicates stolen clock cycle
  dma_store   1 bit    mem[dma_a] := dma_d
  dma_fetch   1 bit    m_out = mem[dma_a]
  dma_reg     1 bit    x = reg[dma_a]  (least significant 4 bits
  dma_a       16 bits  address
  dma_d       16 bits  data

-}

---------------------------------------------------------------------------
-- Configuration
---------------------------------------------------------------------------

writeFileName   = "M1/tempLogWrite.txt"   -- trap write output
logCircFileName = "M1/tempLogCircuit.txt" -- circuit simulation output

---------------------------------------------------------------------------
-- Main
---------------------------------------------------------------------------

-- The main program clears the log files, processes the command line
-- arguments (if any), and then starts the circuit simulator.

main :: IO ()
main = driver $ do
  printLine "Sigma16 M1 system starting, enter help for commands"
  lift $ writeFile writeFileName "" -- ensure it's empty
  lift $ writeFile logCircFileName "" -- ensure it's empty
  handleCommandLineArgs
  simulateCircuit

-- There is one optional command line argument; if it exists it is
-- treated as an object file name and an implicit boot command is
-- executed.

handleCommandLineArgs :: StateT (SysState DriverState) IO ()
handleCommandLineArgs = do
  args <- lift getArgs
  case args of
    [] -> return ()
    (a:_) -> bootObjectFile a

---------------------------------------------------------------------------
-- Driver state for M1
---------------------------------------------------------------------------

-- The Driver State is kept in the system state (SysState
-- DriverState).  The simulation driver keeps notes on what the
-- processor circuit is doing in the DriverState..

data DriverState = DriverState
  { processorMode :: ProcessorMode
  , displacement :: (Int,[Int])      -- (cycle, displacement)
  , effAddr :: [Int]                 -- effective address
  , rfloads :: [(Int,[Int],[Int])]   -- [(cycle,reg,value)]
  , memStores :: [(Int,[Int],[Int])] -- [(cycle,addr,value)]
  , jumps :: [(Int,[Int],[Int])]       -- [(cycle,jumped,pcvalue)]
  , trap :: Bool
  }                     -- has a trap just been executed?
  deriving Show

-- Most actions in the processor require many clock cycles.  The
-- Processor Mode indicates what general activity is going on.  The
-- most important modes are Booting and Running.  When the system
-- starts up, the mode is Idle.

data ProcessorMode
  = Idle
  | Booting
  | Resetting
  | Running
  deriving (Eq, Read, Show)

-- Zero as a 16-bit word with bits represented as Int
dsWord0 :: [Int]
dsWord0 = take 16 (repeat 0)

initDriverState :: DriverState
initDriverState =
  DriverState
    { processorMode = Idle
    , displacement = (0, dsWord0)
    , effAddr = dsWord0
    , rfloads = []
    , memStores = []
    , jumps = []
    , trap = False
    }

-- Access the processor mode

setMode :: ProcessorMode -> StateT (SysState DriverState) IO ()
setMode m = do
--  printLine ("Setting mode to " ++ show m)
  mds <- getUserState
  case mds of
    Just ds -> do
      let ds' = ds { processorMode  = m }
      s <- get
      put (s {userState = Just ds'})
      return ()
    Nothing -> do
      printError "setMode, getUserState returned Nothing"
      return ()

getProcessorMode :: StateT (SysState DriverState) IO ProcessorMode
getProcessorMode = do
  s <- get
  let mds = userState s
  case mds of
    Nothing -> do printLine "DriverState not defined"
                  return Idle
    Just ds -> return (processorMode ds)

setDisplacement :: (Signal a, Static a) =>
   DriverState -> [[a]] -> DriverState
setDisplacement s [w] =
  s {displacement = (0, map sigInt w)}

showDisplacement :: DriverState -> String
showDisplacement s =
  let (c,d) = displacement s
--  in "displacement " ++ ints16hex4 d
--       ++ " loaded in cycle " ++ show c ++ "\n"
  in ints16hex4 d

-- Record and display the effective address

setEffAddr :: (Signal a, Static a) =>
   DriverState -> [[a]] -> DriverState
setEffAddr s [x] =
  s {effAddr = map sigInt x}

showEffAddr :: DriverState -> String
showEffAddr s = ints16hex4 (effAddr s)

-- Record and display loads to the register file

setRfLoad :: (Signal a, Static a)
  => [[a]] -> StateT (SysState DriverState) IO ()
setRfLoad [r,x] = do
  c  <- getClockCycle
  s  <- get
  case userState s of
    Nothing -> return ()
    Just ds -> do
      let xs = rfloads ds
      let ds' = ds {rfloads = (c, map sigInt r, map sigInt x) : xs}
      put $ s {userState = Just ds'}

clearRfLoads :: (Signal a, Static a) =>
  DriverState -> [[a]] -> DriverState
clearRfLoads s _ = s {rfloads = []}

showRfLoads :: DriverState -> String
showRfLoads s = concat (map f (reverse (rfloads s)))
  where f (c,r,x) =
          "R" ++ show (intsInt r) ++ " := " ++  ints16hex4 x
            ++ " was loaded in cycle " ++ show c ++ "\n"
--            ++ " was loaded\n"

-- Record and display stores to the memory

setMemStore :: (Signal a, Static a)
  => [[a]] -> StateT (SysState DriverState) IO ()
setMemStore [a,x] = do
  c <- getClockCycle
  s <- get
  case userState s of
    Nothing -> return ()
    Just  ds -> do
      let xs = memStores ds
      let ds' = ds {memStores = (c, map sigInt a, map sigInt x) : xs}
      put $ s {userState = Just ds'}

clearMemStores :: (Signal a, Static a) =>
  DriverState -> [[a]] -> DriverState
clearMemStores s _ = s {memStores = []}

showMemStores :: DriverState -> String
showMemStores s = concat (map f (reverse (memStores s)))
  where f (c,a,x) =
          "mem[" ++ ints16hex4 a ++ "] := " ++  ints16hex4 x
            ++ " was stored in cycle " ++ show c ++ "\n"

-- When the driver discovers that a trap has executed, it uses setTrap
-- to record this in the driver state.  The termination predicate uses
-- this value to decide when to stop the simulation.

setTrap :: DriverState -> [a] -> DriverState
setTrap s _ = s {trap = True}

---------------------------------------------------------------------------
-- Cycle looper Core???
---------------------------------------------------------------------------

-- Simulate clock cycles repeatedly, stopping when a termination
-- predicate is satisfied.  This is a tail recursion.

cycleLooper :: StateT (SysState DriverState) IO ()
cycleLooper = do
  s <- get
  if checkFlag (flagTable s) (breakpointKey s)
--      || cycleCountSinceClear s >= 10000
    then do
      clearCycleCount
      cycle <- getClockCycle
      m1ClockCycle -- display the cycle where the breakpoint is satisfied
      putBufLogStrLn (take 72 (repeat '-'))
      putBufLogStrLn ("*** Breakpoint " ++ breakpointKey s
                         ++ " in cycle " ++ show cycle ++ " ***")
      putBufLogStrLn (take 72 (repeat '-'))
      return ()
    else do m1ClockCycle
            s <- get
            case halted s of
              True -> do
                printLine "Processor has halted"
                return ()
              False -> cycleLooper
            
---------------------------------------------------------------------------
-- M1 clock cycle
---------------------------------------------------------------------------

m1ClockCycle :: Operation DriverState
m1ClockCycle = do
  s <- get
  let i = cycleCount s
  putBufLogStrLn ('\n' : take 80 (repeat '-'))
  putBufLogStr ("Cycle " ++ show i ++ ".  ")
  establishM1inputs
  pm <- getProcessorMode
  putBufLogStr (show pm)
  putBufLogStrLn (if halted s then "  Halted" else "")
  runFormat FormatNormal
  clockTick
  s <- get
  let f = continueOp s
--  lift $ putLogStrLn "About to run continuation"
  f
--  lift $ putLogStrLn "The continuation has returned"
  s <- get
  put s {continueOp = noContinuation}
--  lift $ putLogStrLn "Cleared continuation"
--  liftIO $ putLogStrLn ("m1ClockCycle finished")

---------------------------------------------------------------------------
-- Main M1 driver
---------------------------------------------------------------------------

simulateCircuit :: StateT (SysState DriverState) IO ()
simulateCircuit = do

-- Inputs
  reset        <- inputBit  "reset"
  io_DMA       <- inputBit  "io_DMA"
  io_memStore  <- inputBit  "io_memStore"
  io_memFetch  <- inputBit  "io_memFetch"
  io_regFetch  <- inputBit  "io_regFetch"
  io_address   <- inputWord "io_address" 16
  io_data      <- inputWord "io_data" 16

-- The M1 circuit
  let io = SysIO {..}  -- define names for system signals
  let  (CtlState {..}, ctl_start, (CtlSig {..}), dp,
        m_sto, m_addr, m_real_addr, m_data, m_out)
         = m1 reset io
  let (r,ccnew) = aluOutputs dp  -- define names for alu subsystem outputs

-- Prepare for memory and register dump  
  setPeek m_out
  setPeek (b dp)
  setPeek (ir dp) -- ***
  
-- Prepare breakpoints  
  setFlagTable
        [ ("reset", reset)
        , ("st_instr_fet",  st_instr_fet)
        , ("st_dispatch",   st_dispatch)
        , ("st_add",        st_add)
        , ("st_sub",        st_sub)
        , ("st_inv",        st_inv)
        , ("st_and",        st_and)
        , ("st_or",         st_or)
        , ("st_xor",        st_xor)
        , ("st_mul0",       st_mul0)
        , ("st_div0",       st_div0)
        , ("st_cmp",        st_cmp)
        , ("st_trap0",      st_trap0)
        , ("st_lea0",       st_lea0)
        , ("st_load0",      st_load0)
        , ("st_store0",     st_store0)
        , ("st_jump0",      st_jump0)
        , ("st_jumpc00",    st_jumpc00)
        , ("st_jumpc10",    st_jumpc10)
        , ("st_jal0",       st_jal0)
        , ("st_loadxi0",    st_loadxi0)
        ]

-- Format the simulation output
  format
    [ string "\nSystem control\n"
    , string "  reset = ", bit reset
    , string "  cpu = ", bit cpu
    , string "  ctl_start = ", bit ctl_start
    , string "\n"
    , string "\nInput/Output\n"
    , string "  io_DMA = ", bit io_DMA
    , string "  io_memStore = ", bit io_memStore
    , string "  io_memFetch = ", bit io_memFetch
    , string "  io_regFetch = ", bit io_regFetch
    , string "\n"
    , string "  io_address = ", binhex io_address
    , string "  io_data = ", binhex io_data
    , string "\n"
    , string "\nControl state\n  "
    , string " st_instr_fet = ", bit dff_instr_fet, bit st_instr_fet
    , string "  st_dispatch = ", bit dff_dispatch, bit st_dispatch
    , string "       st_add = ", bit dff_add, bit st_add
    , string "       st_sub = ", bit dff_sub, bit st_sub
    , string "\n  "
    , string "       st_inv = ", bit dff_inv, bit st_inv
    , string "       st_and = ", bit dff_and, bit st_and
    , string "       st_or  = ", bit dff_or, bit st_or
    , string "       st_xor = ", bit dff_xor, bit st_xor
    , string "\n  "
    , string "      st_mul0 = ", bit dff_mul0, bit st_mul0
    , string "      st_div0 = ", bit dff_div0, bit st_div0
    , string "       st_cmp = ", bit dff_cmp, bit st_cmp
    , string "     st_trap0 = ", bit dff_trap0, bit st_trap0
    , string "\n  "
    , string "      st_lea0 = ", bit dff_lea0, bit st_lea0
    , string "      st_lea1 = ", bit dff_lea1, bit st_lea1
    , string "     st_load0 = ", bit dff_load0, bit st_load0
    , string "\n  "
    , string "     st_load1 = ", bit dff_load1, bit st_load1
    , string "     st_load2 = ", bit dff_load2, bit st_load2
    , string "    st_store0 = ", bit dff_store0, bit st_store0
    , string "    st_store1 = ", bit dff_store1, bit st_store1
    , string "\n  "
    , string "    st_store2 = ", bit dff_store2, bit st_store2
    , string "     st_jump0 = ", bit dff_jump0, bit st_jump0
    , string "     st_jump1 = ", bit dff_jump1, bit st_jump1
    , string "     st_jump2 = ", bit dff_jump2, bit st_jump2
    , string "\n  "
    , string "   st_jumpc00 = ", bit dff_jumpc00, bit st_jumpc00
    , string "   st_jumpc01 = ", bit dff_jumpc01, bit st_jumpc01
    , string "   st_jumpc02 = ", bit dff_jumpc02, bit st_jumpc02
    , string "   st_jumpc10 = ", bit dff_jumpc10, bit st_jumpc10
    , string "\n  "
    , string "   st_jumpc11 = ", bit dff_jumpc11, bit st_jumpc11
    , string "   st_jumpc12 = ", bit dff_jumpc12, bit st_jumpc12
    , string "      st_jal0 = ", bit dff_jal0, bit st_jal0
    , string "      st_jal1 = ", bit dff_jal1, bit st_jal1
    , string "\n  "
    , string "      st_jal2 = ", bit dff_jal2, bit st_jal2
    , string "   st_loadxi0 = ", bit dff_loadxi0, bit st_loadxi0
    , string "   st_loadxi1 = ", bit dff_loadxi1, bit st_loadxi1
    , string "   st_loadxi2 = ", bit dff_loadxi2, bit st_loadxi2
    , string "\n  "
    , string "   st_loadxi3 = ", bit dff_loadxi3, bit st_loadxi3

    , string "\n\nControl signals\n  "
    , string "    ctl_alu_a = ", bit ctl_alu_a
    , string "    ctl_alu_b = ", bit ctl_alu_b
    , string "    ctl_alu_c = ", bit ctl_alu_c
    , string "    ctl_alu_d = ", bit ctl_alu_d
    , string "\n  "
    , string "     ctl_x_pc = ", bit ctl_x_pc
    , string "     ctl_y_ad = ", bit ctl_y_ad
    , string "    ctl_rf_ld = ", bit ctl_rf_ld
    , string "  ctl_rf_ldcc = ", bit ctl_rf_ldcc
    , string "\n  "
    , string "    ctl_rf_pc = ", bit ctl_rf_pc
    , string "    ctl_pc_ld = ", bit ctl_pc_ad
    , string "    ctl_pc_ad = ", bit ctl_pc_ad
    , string "   ctl_rf_alu = ", bit ctl_rf_alu
    , string "\n  "
    , string "    ctl_rf_sd = ", bit ctl_rf_sd
    , string "    ctl_rf_da = ", bit ctl_rf_da
    , string "    ctl_ir_ld = ", bit ctl_ir_ld
    , string "    ctl_pc_ld = ", bit ctl_pc_ld
    , string "\n  "
    , string "    ctl_ad_ld = ", bit ctl_ad_ld
    , string "   ctl_ad_alu = ", bit ctl_ad_alu
    , string "    ctl_ma_pc = ", bit ctl_ma_pc
    , string "      ctl_sto = ", bit ctl_sto

    , string "\n\nALU\n"
    , string "  ALU inputs: "
    , string "  operation = ", bit ctl_alu_a, bit ctl_alu_b, bit ctl_alu_c, bit ctl_alu_d
    , string "  x = ", binhex (x dp)
    , string "  y = ", binhex (y dp)
    , string "  cc = ", binhex (cc dp)
    , string "  ir_d = ", binhex (ir_d dp)
    , string "\n  ALU outputs: "
    , string "  r = ", binhex r
    , string "  ccnew = ", binhex ccnew
    , string "  condcc = ", bit condcc
     
    , string "\n\nDatapath\n  "
    , string "    ir = ", binhex (ir dp)
    , string "    pc = ", binhex (pc dp)
    , string "    ad = ", binhex (ad dp)
    , string "    cc = ", binhex (cc dp)
    , string "\n  "
    , string "     a = ", binhex (a dp)
    , string "     b = ", binhex (b dp)
    , string "     x = ", binhex (x dp)
    , string "     y = ", binhex (y dp)
    , string "\n  "
    , string "     p = ", binhex (p dp)
    , string "     q = ", binhex (q dp)
    , string "     r = ", binhex (r)
    , string "\n  "
    , string "    ma = ", binhex (ma dp)
    , string "    md = ", binhex (md dp)

-- Memory interface
    , string "\n\nMemory\n  "
    , string "  m_sto = ", bit m_sto
    , string "  m_addr = ", binhex m_addr
    , string "  m_real_addr = ", binhex m_real_addr
    , string "  m_data = ", binhex m_data
    , string "  m_out = ", binhex m_out
    , string "\n"

-- ...................................................................
-- Higher level analysis of what happened on this cycle.  The
-- following actions examine various signals in order to detect what
-- is happening in the machine, and they print higher level
-- description.
-- ...................................................................

-- Print a message when the system is reset

    ,  fmtIf reset
           [string ("\n" ++ take 72 (repeat '*') ++ "\n"),
            string "Reset: control algorithm starting",
            string ("\n" ++ take 72 (repeat '*')),
            string "\n"]
           [],

-- When the displacement for an RX instruction is fetched, save
-- it in the simulation driver state

         fmtIf (orw [st_lea1, st_load1, st_store1, st_jump1, st_jumpc01,
                     st_jumpc11, st_jal1, st_loadxi1])
           [setStateWs setDisplacement [(ad dp)],
            string "*** Fetched displacement = ",
            simstate showDisplacement,
            string "\n"
           ]
           [],

-- Record the effective address when it is calculated.  This is the r
-- output of the ALU, and usually will be loaded into the ad register.

         fmtIf (orw [st_lea1, st_load1, st_store1, st_jump1,
                     st_jumpc01, st_jumpc11, st_jal1, st_loadxi1])
         [setStateWs setEffAddr [r]]
           [],

-- Process a load to the register file
         fmtIf (and2 ctl_rf_ld (inv ctl_rf_da))
           [string "Register file update: ",
            string "R",
            bindec 1 (field (ir dp) 4 4),
            string " := ", hex (p dp),
            setStateWsIO setRfLoad [field (ir dp) 4 4, (p dp)],
            string "\n"
            ]
           [],

-- Process a load to the register file that uses ir_sa as the destination
         fmtIf (and2 ctl_rf_ld ctl_rf_da)
           [string "Register file update: ",
            string "R",
            bindec 1 (field (ir dp) 8 4),
            string " := ", hex (p dp),
            setStateWsIO setRfLoad [field (ir dp) 8 4, (p dp)],
            string "\n"
            ]
           [],

-- Process a store to memory
         fmtIf ctl_sto
           [string "Memory store:  ",
            string "mem[",
            binhex m_addr,
            string "] := ", hex m_data,
            setStateWsIO setMemStore [m_addr, m_data]
           ]
           [],

-- If an instruction was completed during this clock cycle, fetch it,
-- decode it, and print it.  The first word of the instruction is in
-- ir, the second word (if it is an RX instruction) is in the
-- displacement field of the simulation driver state.

         fmtIf (and2 ctl_start (inv reset))
           [string ("\n" ++ take 72 (repeat '*') ++ "\n"),
            string "Executed instruction:  ",
            fmtWordsGeneral findMnemonic
              [field (ir dp) 0 4, field (ir dp) 12 4],
            string " ",
            fmtIf (orw [st_add, st_sub, st_cmp, st_trap0])

              [string " R", bindec 1 (field (ir dp) 4 4),    -- RRR format
               string ",R", bindec 1 (field (ir dp) 8 4),
               string ",R", bindec 1 (field (ir dp) 12 4)]
              [string " R", bindec 1 (field (ir dp) 4 4),    -- RX format
               string ",",
               simstate showDisplacement,
               string "[R", bindec 1 (field (ir dp) 8 4), string "]" ], -- ,
--               string "   effective address = ",
--               simstate showEffAddr],
            string "\n",
            simstate showRfLoads,
            setStateWs clearRfLoads [],
            simstate showMemStores,
            setStateWs clearMemStores [],

 --- Describe effect of jumps
            fmtIf st_jumpc02            
              [fmtIf condcc
                 [string "jumpc0 instruction will not jump\n"]
                 [string "jumpc0 instruction is jumping to ",
                  binhex (ad dp), string "\n"]]
              [],

            fmtIf st_jumpc12
               [fmtIf condcc
                 [string "jumpc1 instruction is jumping to ",
                  binhex (ad dp), string "\n"]
                 [string "jumpc1 instruction will not jump\n"]]
               [],

            fmtIf st_jump2
              [string "jump instruction is jumping to ",
               binhex (ad dp), string "\n"]
              [],
            fmtIf st_jal2
              [string "jal instruction is jumping to ",
               binhex (ad dp), string "\n"]
              [],
            
-- Display instruction control registers
            string "Processor state:  ",
            string "  pc = ", binhex (pc dp),
            string "  ir = ", binhex (ir dp),
            string "  ad = ", binhex (ad dp),
            string ("\n" ++ take 72 (repeat '*')),
            string "\n"
              ]
           [],
         
-- If a trap is being executed, indicate this in the simulation driver
-- state, so the driver can terminate the simulation

         fmtIf st_trap0
           [
--             string "\n Reached state trap0\n",
            setStateWs setTrap [],
--            string "\ntrap ir d 3: ", bit (ir dp !! 11),
--            string "\ntrap ir d 2: ", bit (ir dp !! 10),
--            string "\ntrap ir d 1: ", bit (ir dp !! 9),
--            string "\ntrap ir d 0: ", bit (ir dp !! 8),
            call trapInstruction
--            setHalted,
--            string ("\n" ++ take 72 (repeat '*') ++ "\n"),
--            string "System trap request:  Halt\n",
--            string "Processor has halted\n",
--            string (take 72 (repeat '*') ++ "\n")
           ]
           []
    ]

-- This ends definitions of the tools; the driver algorithms starts
-- now
  startCommandLoop
  printLine "M1 run finished"

---------------------------------------------------------------------------
-- Obtain input signal values
---------------------------------------------------------------------------

establishM1inputs :: StateT (SysState DriverState) IO ()
establishM1inputs = do
  mds <- getUserState
  case mds of
    Nothing -> do
      printError "establishM1inputs: empty driver state"
      return ()
    Just ds ->
      case processorMode ds of
        Idle -> do
          return ()
        Booting  -> do
          inp <- getStoredInput
          case inp of
            Just x -> takeInputsFromList x
            Nothing -> do
              setMode Resetting
              establishM1inputs
        Resetting -> do
          takeInputsFromList resettingInputs
          setMode Running
        Running -> takeInputsFromList runningInputs

resettingInputs = "1 0 0 0 0 0 0"
runningInputs   = "0 0 0 0 0 0 0"

-- If there is a list of stored input values, get the first one and
-- remove it.  Each operation that requires DMA is carried out by a
-- function that supplies the required inputs, but does not use any of
-- the input lists.

-- in Driver but should be added to export list (edit Driver)
-- Don't set running to false on end of input

getStoredInput :: StateT (SysState a) IO (Maybe String) -- Core???
getStoredInput = do
  s <- get
  case storedInput s of
    [] -> do
--      put $ s {running = False}  fix this
      return Nothing
    (x:xs) -> do
      put $ s {storedInput = xs}
      putBufLogStrLn ("getStoredInput " ++ x)
      return (Just x)

---------------------------------------------------------------------------
-- Interactive command loop Core??? with table of command/function pairs
---------------------------------------------------------------------------

startCommandLoop :: StateT (SysState DriverState) IO ()
startCommandLoop = do
  s <- get
  put (s {userState = Just initDriverState})
  setMode Idle
  commandLoop

commandLoop :: StateT (SysState DriverState) IO ()
commandLoop = do
  liftIO $ putStr "M1> "
  liftIO $ hFlush stdout
  xs <- liftIO getLine
  let ws = words xs
  if length ws == 0
    then m1ClockCycle
  else if ws!!0 == "help"
    then printHelp
  else if ws!!0 == "boot"
    then do
      let objFileArg = safeReadEltString ws 1
      bootObjectFile objFileArg
  else if ws!!0 == "run"
  then cycleLooper  --  then startSimulationLooper
  else if ws!!0 == "cycle"
    then m1ClockCycle
  else if ws!!0 == "mem"
    then do
      let start = safeReadEltInt ws 1
      let end = safeReadEltInt ws 2
      dumpMem start end
  else if ws!!0 == "regs"
    then dumpRegFile
  else if ws!!0 == "break"
    then do
      let key = safeReadEltString ws 1
      setBreakpoint key
  else if ws!!0 == "logging"
    then do
      let flag = safeReadEltString ws 1
      cmdLogging flag
  else if ws!!0 == "dmasigs"
    then do
      let flag = safeReadEltString ws 1
      cmdDmaSigs flag
  else if ws!!0 == "quit" || ws!!0 == "q"
    then do
      s <- get
      put (s {running = False})
  else liftIO $ putStrLn "Invalid command, enter help for list of commands"
  s <- get
  case running s of
    True -> commandLoop
    False -> return ()

---------------------------------------------------------------------------
-- Commands
---------------------------------------------------------------------------

cmdDmaSigs :: String -> StateT (SysState DriverState) IO ()
cmdDmaSigs flag = do
  if flag == "on"
    then do s <- get
            put s {formatMode = FormatFull}
  else if flag == "off"
    then do s <- get
            put s {formatMode = FormatNormal}
  else lift $ putStrLn "Usage: dmasigs on or dmasigs off"
  return ()

safeReadEltInt :: [String] -> Int -> Int
safeReadEltInt ws i =
  if length ws > i && i >= 0
    then read (ws!!i)
    else 0

safeReadEltString :: [String] -> Int -> String
safeReadEltString ws i =
  if length ws > i && i >= 0
    then ws!!i
    else ""

setBreakpoint :: String -> StateT (SysState a) IO ()
setBreakpoint key = do
  s <- get
  put $ s {breakpointKey = key}

cmdReset :: Command DriverState
cmdReset _ = doReset

-- unimplemented

doReset :: Operation a
doReset = do
  printLine "Resetting unimplemented"
--  selectInputList  "reset"

resetData :: [String]
resetData =  ["1 0 0 0 0 0 0"]

---------------------------------------------------------------------------
-- Memory access
---------------------------------------------------------------------------

--   Use generic cycle function ???

peekMem :: Int -> StateT (SysState DriverState) IO [Bool]
peekMem addr = do
--  let displayFullSignals = False
  s <- get
  let mode = formatMode s
  let peekmode =
        case mode of
          FormatNormal -> FormatQuiet
          FormatFull -> FormatFull
          FormatQuiet -> FormatQuiet
  let displayFullSignals =
        case peekmode of
          FormatNormal -> False
          FormatFull -> True
          FormatQuiet -> False
  let i = cycleCount s
  let inps = "0 1 0 1 0 " ++ show addr ++ " 0"
  takeInputsFromList inps

  conditional displayFullSignals $ do
    putBufLogStrLn (take 80 (repeat '-'))
    putBufLogStr ("Cycle " ++ show i ++ ".  ")
    putBufLogStrLn ("  system inputs = " ++ inps)

  s <- get
  let ps = peekList s
  let a = ps!!0  -- m_out
  let bs = map current a
  putBufLogStrLn ("*** Cycle " ++ show i
                   ++ ": I/O fetch Mem[" ++ inthexok 4 addr ++ "] = "
                   ++ bitsHex 4 bs)
  runFormat peekmode
  clockTick
  return bs

-- Generate control signals that use DMA to perform mem[a] := x

--   Use generic cycle function ???

pokeMem :: Int -> Int -> StateT (SysState DriverState) IO ()
pokeMem a x = do
  let abs = intToBits a 16
  let xbs = intToBits x 16
  let displayFullSignals = True
  s <- get
  let i = cycleCount s
  let inps = "0 1 1 0 0 " ++ show a ++ " " ++ show x
  takeInputsFromList inps

  conditional displayFullSignals $ do
    putBufLogStrLn (take 80 (repeat '-'))
  putBufLogStr ("Cycle " ++ show i ++ ".  ")
  putBufLogStrLn $ "pokeMem a = " ++ bitsHex 4 abs
    ++ " value = " ++ bitsHex 4 xbs
  conditional displayFullSignals $ do
    putBufLogStrLn ("inps = " ++ inps)
    printInPorts
    printOutPorts
  putBufLogStrLn ("*** I/O store Mem[" ++ bitsHex 4 abs ++ "] := "
                     ++ bitsHex 4 xbs)
  runFormat FormatNormal
  clockTick

-- Return list of len memory words from start address

readMemBlock :: Int -> Int -> [Int]
  -> StateT (SysState DriverState) IO [Int]
readMemBlock start len xs = do
  if len == 0
    then return (reverse xs)
    else do x <- peekMem start
            readMemBlock (start+1) (len-1) (bitsBin x: xs)

-- Store len data words into memory from start address a

writeMemBlock :: Int -> [Int] -> StateT (SysState DriverState) IO ()
writeMemBlock a [] = return ()
writeMemBlock a (x:xs) = do
  pokeMem a x
  writeMemBlock (a+1) xs

---------------------------------------------------------------------------
-- Register access
---------------------------------------------------------------------------

peekReg :: Int -> StateT (SysState DriverState) IO [Bool]
peekReg regnum = do
  s <- get
  let mode = formatMode s
  let peekmode =
        case mode of
          FormatNormal -> FormatQuiet
          FormatFull -> FormatFull
          FormatQuiet -> FormatQuiet
--  lift $ putStrLn ("peekReg mode = " ++ show mode)
  let displayFullSignals =
        case peekmode of
          FormatNormal -> False
          FormatFull -> True
          FormatQuiet -> False
  let i = cycleCount s
  let inps = "0 1 0 0 1 " ++ show regnum ++ " 0"
  takeInputsFromList inps

  conditional displayFullSignals $ do
    putBufLogStrLn (take 80 (repeat '-'))
    putBufLogStr ("Cycle " ++ show i)
    putBufLogStrLn ("  system inputs = " ++ inps)
    autoPrintPorts

  s <- get
  let ps = peekList s
  let b = ps!!1  --  output b from regfile
  let bs = map current b
  putBufLogStrLn ("*** Cycle " ++ show i ++ ": I/O fetch R"
                   ++ show regnum ++ " = " ++ bitsHex 4 bs)
  runFormat peekmode
  clockTick
  return bs

---------------------------------------------------------------------------
-- Boot
---------------------------------------------------------------------------

-- Given filename, attempt to read the file, parse the object code,
-- and boot it

bootObjectFile :: String -> StateT (SysState DriverState) IO ()
bootObjectFile fname = do
      lift $ putStrLn ("bootObjectFile " ++ fname)
      setMode Booting
      prefixFile <- lift $ maybeRead "M1/progdir.txt"
      let basePath =
            case prefixFile of
              Nothing -> defaultProgramPath ++ fname
              Just p -> lines p !! 0 ++ fname
      let fullPath = basePath ++ ".obj.txt"
      printLine ("Reading object file " ++ fullPath)
      putBufLogStrLn ("Reading object file " ++ fullPath)
      objectCode <- lift $ readObjectCode fullPath
      printLine ("Object code is " ++ show objectCode)
      let bootData = generateBootControls objectCode
      putStoredInput bootData
      printLine ("Boot system inputs = " ++ show bootData)
  
-- Generate control signals to boot object code

generateBootControls :: [Int] -> [String]
generateBootControls code =
  let f i x = "0 1 1 0 0 " ++ show i ++ " " ++ show x
      inps = zipWith f [0..] code
  in inps

-- Store object code into memory
-- Core ???
putStoredInput :: [String] -> StateT (SysState a) IO ()
putStoredInput storedInput = do
  s <- get
  put $ s {storedInput}

---------------------------------------------------------------------------
-- Perform action requested by trap instruction
---------------------------------------------------------------------------

trapInstruction :: StateT (SysState DriverState) IO ()
trapInstruction = do
  putBufLogStrLn
    "\nSystem simulation driver is handling a trap request"
  s <- get
  let ps = peekList s
  let irsigs = ps !! 2
  let irbits = map current irsigs
  let rdbits = take 4 (drop 4 irbits)
  let rabits = take 4 (drop 8 irbits)
  let rbbits = take 4 (drop 12 irbits)
  let rd = boolsInt rdbits
  let ra = boolsInt rabits
  let rb = boolsInt rbbits
--  liftIO $ putLogStrLn ("trap rd = " ++ show rd)
--  liftIO $ putLogStrLn ("trap ra = " ++ show ra)
--  liftIO $ putLogStrLn ("trap rb = " ++ show rb)
  putBufLogStrLn ("Executing trap R" ++ show rd ++ ",R"
                     ++ show ra ++ ",R" ++ show rb)
  dval <- peekReg rd
  aval <- peekReg ra
  bval <- peekReg rb
  let d = bitsBin dval
  let a = bitsBin aval
  let b = bitsBin bval
--  liftIO $ putLogStrLn ("trap arg d = " ++ show d)
--  liftIO $ putLogStrLn ("trap arg a = " ++ show a)
--  liftIO $ putLogStrLn ("trap arg b = " ++ show b)

  putBufLogStrLn ("trap arg d = " ++ bitsHex 4 dval)
  putBufLogStrLn ("trap arg a = " ++ bitsHex 4 aval)
  putBufLogStrLn ("trap arg b = " ++ bitsHex 4 bval)

  if d==0 -- halt
    then do putBufLogStrLn "Trap arg d = 0 so this is a Halt request"
            s <- get
            put (s {halted = True})
            putBufLogStrLn ("\n" ++ take 72 (repeat '*'))
            putBufLogStrLn "System trap request:  Halt"
            putBufLogStrLn "Processor has halted"
            putBufLogStrLn (take 72 (repeat '*') ++ "\n")
            return ()
            
  else if d==1 -- read
    then do putBufLogStrLn "Trap arg d = 1 so this is a Read request"
--            let unusedInputData = "a"
--            newInputData <- lift $ readFromUser b unusedInputData
--            let availableInputData = unusedInputData ++ newInputData
--            let size = min b (length availableInputData)
--            let actualInputData = take size availableInputData
--            let newUnusedInputData = drop size availableInputData
            inputData <- lift $ readFromUser b ""
            let xs = map ord inputData
            putBufLogStrLn $ "trap read storing " ++ show xs
            writeMemBlock a xs
            return ()

  else if d==2 -- write
    then do putBufLogStrLn "Trap arg d = 2 so this is a Write request"
            putBufLogStrLn ("Fetching " ++ show b ++ " characters"
                              ++ " starting from memory address "
                              ++ bitsHex 4 aval)
            xs <- readMemBlock a b []
            let xs' = map restrictToLatin1 xs
            let ys = map chr xs'
--            lift $ putLogStrLn ("data to write = " ++ show xs)
--            lift $ putLogStrLn ("data to write = " ++ ys)
            writeLogOutput ys
            return ()
  else   do putBufLogStrLn ("Ignoring undefined trap code " ++ show d)
            return ()

  putBufLogStrLn "trap request finished"

---------------------------------------------------------------------------
-- Dump memory from start to end address
---------------------------------------------------------------------------

restrictToLatin1 :: Int -> Int
restrictToLatin1 x
  | x < 255 = x
  | otherwise = 97  -- replace with 'a'

dumpMem :: Int -> Int -> StateT (SysState DriverState) IO ()
dumpMem start end = do
  case start <= end of
    True -> do
      peekMem start
      dumpMem (start+1) end
    False -> return ()

readFromUser :: Int -> String -> IO String
readFromUser n buf = do
  putStrLn "\n*****"
  putStrLn ("Read request, buffer size = " ++ show n ++ " characters")
  putStrLn ("Input will be padded with spaces, and extra characters ignored")
  xs <- hGetLine stdin
  let ys = take n (xs ++ repeat ' ')
  putStrLn ("Input data = <" ++ ys ++ ">")
  putStrLn "\n*****"
  return ys
--  putLogStrLn ("Read request, buffer size = " ++ show n)
--  if length buf > 0
--    then putLogStrLn ("Data you enter will follow buffered data: <"
--    ++ buf ++ ">")
--    else return ()                  

---------------------------------------------------------------------------
-- Dump register file
---------------------------------------------------------------------------

dumpRegFile :: StateT (SysState DriverState) IO ()
dumpRegFile = do
  let f i =
        case i <= 15 of
          True -> do
            x <- peekReg i
--            putBufLogStrLn ("reg " ++ show i ++ " = " ++ show x)
            f (i+1)
          False -> return ()
  f 0

-- table driven, Core ???

printHelp :: StateT (SysState a) IO ()
printHelp = do
  printLine "Commands for the M1 driver"
  printLine "  (blank)     -- perform one clock cycle"
  printLine "  cycle       -- perform one clock cycle"
  printLine "  boot fname  -- boot object code from file"
  printLine "  run         -- repeat clock cycles until halt or break"
  printLine "  regs        -- display contents of the register file"
  printLine "  mem a b     -- display memory from address a to b"
  printLine "  break FLAG  -- run will stop when FLAG = 1 (see list below)"
  printLine "  logging on"
  printLine "  logging off"
  printLine "  dmasigs on"
  printLine "  dmasigs off"
  printLine "  quit        -- return to ghci or shell prompt"
  printLine "  help        -- list the commands"
  printLine "The following signals can be used as break FLAG:"
  s <- get
  printLine (concat (map ((' ':) . fst) (flagTable s)))

---------------------------------------------------------------------------
-- Decoding instructions
---------------------------------------------------------------------------

-- When an instruction is decoded, findMnemonic returns the assembly
-- language mnemonic for the instruction, given the opcode.  The
-- opcode consists of the op field of the instruction, as well as the
-- sb field.  For RRR instructions, the op field determines the
-- instruction.  For RX instructions, the op is 15, which indicates an
-- escape to the sb field.

findMnemonic :: [[Int]] -> String
findMnemonic [opfield, bfield] =
  let op = intsInt opfield
      b = intsInt bfield
      mnemonics_RRR =
        ["add", "sub", "mul", "div",
         "cmp", "inv", "and", "or",
         "xor", "nop", "nop", "nop",
         "trap", "nop", "expandExp", "expandRX"]
      mnemonics_RX =
        ["lea",    "load",   "store", "jump",
         "jumpc0", "jumpc1", "jal",   "loadxi",
         "nop",    "nop",    "nop",   "nop",
         "nop",    "nop",    "nop",   "nop"]
  in if op==15
       then mnemonics_RX !! b
       else mnemonics_RRR !! op

---------------------------------------------------------------------------
-- Output from trap write requests
---------------------------------------------------------------------------

-- combine with circuit signal logging, core???

writeLogOutput :: String -> StateT (SysState b) IO ()
writeLogOutput xs = do
  let endsWithNewline = xs !! (length xs - 1) == '\n'
  putBufLogStr ("\ntrap write " ++ show (length xs) ++ " characters")
  if endsWithNewline
    then putBufLogStrLn " (ends with newline):"
    else putBufLogStrLn ":"
  putBufLogStrLn (take 75 (repeat '~'))
  putBufLogStr xs
  if endsWithNewline
    then return ()
    else putBufLogStr "\n"
  putBufLogStrLn (take 75 (repeat '~'))
  lift $ appendFile writeFileName xs
