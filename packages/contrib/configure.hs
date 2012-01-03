#! /usr/bin/env runhaskell

import System.Environment(getEnv)

main = do
    nppath <- getEnv "EASYVISION"
    putStrLn $ "EASYVISION path: " ++ nppath
    writeFile "contrib.buildinfo" $ 
              "extra-lib-dirs: "++nppath++"/lib\n"

