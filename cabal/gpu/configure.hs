#! /usr/bin/env runhaskell

import System.Environment(getEnv)

main = do
    cudapath <- getEnv "CUDA"
    putStrLn cudapath
    writeFile "imagproc-gpu.buildinfo" $ 
              "extra-lib-dirs: "++cudapath++"\n"
