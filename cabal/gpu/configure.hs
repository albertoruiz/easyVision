#! /usr/bin/env runhaskell

import System.Environment(getEnv)

main = do
    cudapath <- getEnv "CUDA"
    putStrLn $ "CUDA path: " ++ cudapath
    writeFile "imagproc-gpu.buildinfo" $ 
              "extra-lib-dirs: "++cudapath++"\n"
