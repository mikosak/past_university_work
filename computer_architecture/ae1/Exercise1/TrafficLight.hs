-- TrafficLight: traffic light state machine circuit

-- Delay element method is used to represent each possible
-- state of the traffic lights.

module TrafficLight where
import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.Combinational

-- This counter stores up to a 16 bit word. It adds one to
-- the counter when 'trigger' is one.

count16 :: CBit a => a -> a -> [a]
count16 reset trigger = [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15]
  where (c0,x0) = cbit reset c1 
        (c1,x1) = cbit reset c2 
        (c2,x2) = cbit reset c3 
        (c3,x3) = cbit reset c4 
        (c4,x4) = cbit reset c5 
        (c5,x5) = cbit reset c6 
        (c6,x6) = cbit reset c7 
        (c7,x7) = cbit reset c8 
        (c8,x8) = cbit reset c9 
        (c9,x9) = cbit reset c10
        (c10,x10) = cbit reset c11
        (c11,x11) = cbit reset c12
        (c12,x12) = cbit reset c13
        (c13,x13) = cbit reset c14
        (c14,x14) = cbit reset c15
        (c15,x15) = cbit reset trigger

cbit :: CBit a => a -> a -> (a,a)
cbit reset cin = (cout,s)
  where
    s = dff (mux1 reset s' zero)
    (cout,s') = halfAdd cin s

-- This simulates a simple traffic light, following a 
-- preprogrammed sequence.

controller1 :: CBit a => a -> (a,a,a)
controller1 reset = (green,amber,red)
  where
    green0 = dff (or2 reset amber1)
    green1 = dff (and2 reset' green0)
    green2 = dff (and2 reset' green1)
    amber0 = dff (and2 reset' green2)
    red0 = dff (and2 reset' amber0)
    red1 = dff (and2 reset' red0)
    red2 = dff (and2 reset' red1)
    amber1 = dff (and2 reset' red2)
    reset' = inv reset

    red = or3 red0 red1 red2
    amber = or2 amber0 amber1
    green = or3 green0 green1 green2

-- This circuit adds functionality such as pedestrian lights
-- and a counter for the number of pedestrian ligh requests.

controller2 :: CBit a => a -> a -> (a,a,a,a,a,[a])
controller2 reset walkRequest = (green,amber,red,wait,walk,walkCount)
  where
    greenWait = dff (and2 (or2 walkRequest' amberWait1) (or3 reset greenWait amberWait1))
    amberWait0 = dff (and3 walkRequest reset' greenWait)
    redWalk0 = dff (and2 reset' amberWait0)
    redWalk1 = dff (and2 reset' redWalk0)
    redWalk2 = dff (and2 reset' redWalk1)
    amberWait1 = dff (and2 reset' redWalk2)

    reset' = inv reset
    walkRequest' = inv walkRequest
    count = count16 reset walkRequest

    red = or3 redWalk0 redWalk1 redWalk2
    amber = or2 amberWait0 amberWait1
    green = greenWait
    wait = or2 green amber
    walk = red
    walkCount = count

