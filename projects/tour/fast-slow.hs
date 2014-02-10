-- space leak tests
-- proc2 and proc3 were leaky with the initial async arrow definition
--
-- ej: $ ./fast-slow ../../data/videos/rot4.avi

{-# LANGUAGE Arrows #-}

import Vision.GUI
import Image.Processing
import Util.Misc(splitEvery)

fast n = map head . splitEvery n

main = run $ arr rgb >>> proc2

proc1, proc2, proc3 :: ITrans (Image RGB) (Image RGB)

-- "normal" order: show orig, filter, show fast, output fast
proc1 = proc x -> do
    observe "orig" id -< x
    g <- arrL (fast 10) -< x
    observe "fast" id -< g
    returnA -< g

-- show fast but output orig
proc2 = proc x -> do
    observe "orig" id -< x
    g <- arrL (fast 10) -< x
    observe "fast" id -< g
    returnA -< x

-- show orig after fast
proc3 = proc x -> do
    g <- arrL (fast 10) -< x
    observe "fast" id -< g
    observe "orig" id -< x
    returnA -< g

