#! /usr/bin/env runhaskell

import Image.Devel(getInclude)

main = do
    incdir <- getInclude
    writeFile "hVision-custom.buildinfo" $ unlines
        [ incdir
        ]

