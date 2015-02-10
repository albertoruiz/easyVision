#! /usr/bin/env runhaskell

import Image.Devel(getInclude)
import System.Directory(doesFileExist)

main = do
    incdir <- getInclude
    opencv24 <- doesFileExist "/usr/include/opencv2/nonfree/features2d.hpp"
    let def = if opencv24
                then ["cc-options: -DOPENCV24"]
                else []
    writeFile "hVision-opencv.buildinfo" $ unlines $
        [ incdir
        ] ++ def

