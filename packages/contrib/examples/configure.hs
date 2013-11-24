#! /usr/bin/env runhaskell

import Image.OldCore(getDataFileName)
import Data.List.Split(splitOn)

main = do
    fw <- getDataFileName "src/Image/Devel/wrappers.h"
    writeFile "hVision-contrib.buildinfo" $ unlines
        [ "include-dirs: " ++ head (splitOn "/wrappers.h" fw)
        ]

