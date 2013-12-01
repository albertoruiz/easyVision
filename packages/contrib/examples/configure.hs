#! /usr/bin/env runhaskell

import Image.Devel(getInclude)

main = do
    incdir <- getInclude
    writeFile "hVision-contrib.buildinfo" $ unlines
        [ incdir
        ]

