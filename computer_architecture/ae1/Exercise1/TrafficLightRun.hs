-- TrafficLightRun: simulation driver for traffic light state machine
-- Usage: ghc -e main TrafficLightRun.hs

module Main where
import HDL.Hydra.Core.Lib
import TrafficLight

main :: IO ()
main = do
  putStrLn ('\n' : take 70 (repeat '*'))
  putStrLn "Running controller1 on testData1"
  controller1Run testData1
  putStrLn ('\n' : take 70 (repeat '*'))
  putStrLn "Running controller2 on testData2"
  controller2Run testData2

testData1 :: [String]
testData1 =
--  reset   state    output
--  ~~~~~~~~~~~~~~~~~~~~~~~
  [ "1"  -- reset'  None
  , "0"  -- green0  Green
  , "0"  -- green1  Green
  , "0"  -- green2  Green
  , "0"  -- amber0  Amber
  , "0"  -- red0    Red
  , "0"  -- red1    Red
  , "0"  -- red2    Red
  , "0"  -- amber1  Amber
  , "0"  -- green0  Green (repeated)
  , "0"  -- green1  Green
  , "0"  -- green2  Green
  , "0"  -- amber0  Amber
  , "0"  -- red0    Red
  , "0"  -- red1    Red
  , "0"  -- red2    Red
  , "0"  -- amber1  Amber
  , "0"  -- green0  Green (repeated)
  ]

testData2 :: [String]
testData2 = 
--  reset  walkRequest  Green  Amber  Red  Wait  Walk  walkCount
--  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  [  "1      0" --          0      0    0     0     0          0
  ,  "0      0" --          1      0    0     1     0          0
  ,  "0      0" --          1      0    0     1     0          0
  ,  "0      0" --          1      0    0     1     0          0
  ,  "0      0" --          1      0    0     1     0          0
  ,  "0      1" --          1      0    0     1     0          0
  ,  "0      0" --          0      1    0     1     0          1
  ,  "0      0" --          0      0    1     0     1          1
  ,  "0      0" --          0      0    1     0     1          1
  ,  "0      0" --          0      0    1     0     1          1
  ,  "0      0" --          0      1    0     1     0          1
  ,  "0      0" --          1      0    0     1     0          1
  ,  "0      0" --          1      0    0     1     0          1
  ,  "0      0" --          1      0    0     1     0          1
  ,  "0      0" --          1      0    0     1     0          1
  ,  "0      0" --          1      0    0     1     0          1
  ,  "0      0" --          1      0    0     1     0          1
  ,  "0      1" --          1      0    0     1     0          1
  ,  "0      1" --          0      1    0     1     0          2
  ,  "0      1" --          0      0    1     0     1          3
  ,  "0      0" --          0      0    1     0     1          4
  ,  "0      0" --          0      0    1     0     1          4
  ,  "0      0" --          0      1    0     1     0          4
  ,  "0      0" --          1      0    0     1     0          4
  ,  "0      0" --          1      0    0     1     0          4
  ,  "0      1" --          1      0    0     1     0          4
  ,  "0      0" --          0      1    0     1     0          5
  ,  "0      0" --          0      0    1     0     1          5
  ,  "1      0" --          0      0    1     0     1          5
  ,  "0      0" --          1      0    0     1     0          0
  ,  "0      0" --          1      0    0     1     0          0
  ,  "0      0" --          1      0    0     1     0          0
  ,  "0      0" --          1      0    0     1     0          0
  ,  "0      1" --          1      0    0     1     0          0
  ,  "0      1" --          0      1    0     1     0          1
  ,  "0      1" --          0      0    1     0     1          2
  ,  "0      1" --          0      0    1     0     1          3
  ,  "0      1" --          0      0    1     0     1          4
  ,  "0      1" --          0      1    0     1     0          5
  ,  "0      1" --          1      0    0     1     0          6
  ,  "0      0" --          1      0    0     1     0          7
  ,  "0      0" --          1      0    0     1     0          7
  ]

-- Driver for controller1
controller1Run :: [String] -> IO ()
controller1Run xs = driver $ do

-- Input data
  useData xs

-- Inputs
  reset <- inputBit "reset"

-- Circuit
  let (green, amber, red) = controller1 reset

-- Outputs
  outputBit "Green" green
  outputBit "Amber" amber
  outputBit "Red" red

-- Run
  runSimulation

-- Driver for controller2
controller2Run :: [String] -> IO ()
controller2Run xs = driver $ do

-- Input data
  useData xs

-- Inputs
  reset <- inputBit "reset"
  walkRequest <- inputBit "walkRequest"

-- Circuit
  let (green, amber, red, wait, walk, walkCount) = controller2 reset walkRequest

-- Outputs
  outputBit "Green" green
  outputBit "Amber" amber
  outputBit "Red" red
  outputBit "Wait" wait
  outputBit "Walk" walk
  outputWord "Count" walkCount

-- Run
  runSimulation
