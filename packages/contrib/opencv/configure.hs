#! /usr/bin/env runhaskell

import Image.Devel(getInclude)

main = do
    incdir <- getInclude
    writeFile "hVision-opencv.buildinfo" $ unlines
        [ incdir
        ]

