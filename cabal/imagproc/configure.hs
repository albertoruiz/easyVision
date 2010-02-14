#! /usr/bin/env runhaskell

import System.Environment(getEnv)

main = do
    ipppath <- getEnv "IPP"
    putStrLn ipppath
    writeFile "imagproc.buildinfo" $
              "include-dirs: "++ipppath++"/include\n"
           ++ "extra-lib-dirs: "++ipppath++"/sharedlib\n"
