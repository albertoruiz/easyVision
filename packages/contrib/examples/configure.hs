#! /usr/bin/env runhaskell

import Image.Devel(getDataFileName)
import Data.List.Split(splitOn)

main = do
    fw <- getDataFileName "src/Image/Devel/wrappers.h"
    writeFile "contrib.buildinfo" $ unlines
        [ "include-dirs: " ++ head (splitOn "/wrappers.h" fw)
        ]

