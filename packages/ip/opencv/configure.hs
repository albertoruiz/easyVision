#! /usr/bin/env runhaskell

import Image.Devel(getInclude)
import System.Directory(doesFileExist)
import Data.List(isPrefixOf)
import System.Process

main = do
    incdir <- getInclude
    (_,version,_) <- readProcessWithExitCode "pkg-config" ["opencv","--modversion"] ""
    putStrLn $ "opencv "++ version
    let ver = flip isPrefixOf version
    let def = if ver "3"
                then [ "cc-options: -DOPENCV3"
                     , "include-dirs: /usr/local/include /usr/local/include/opencv"
                     , "extra-lib-dirs: /usr/local/lib"
                     , "extra-libraries: opencv_core opencv_calib3d opencv_imgproc opencv_video opencv_objdetect opencv_videoio  opencv_xfeatures2d opencv_highgui"
                     ]
                else if ver "2.4"
                      then [ "cc-options: -DOPENCV24"
                           , "pkgconfig-depends: opencv"
                           ]
                      else ["pkgconfig-depends: opencv"
                           ]
    writeFile "hVision-opencv.buildinfo" $ unlines $ def ++ [ incdir ]

