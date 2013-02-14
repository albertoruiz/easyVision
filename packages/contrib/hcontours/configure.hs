#! /usr/bin/env runhaskell

import System.Environment(getEnv)

import Distribution.System(buildArch,Arch(..))

arch = if buildArch == X86_64 then "/lib/lib64" else "/lib/lib32"

main = do
    lib <- getEnv "EASYVISION"
    putStrLn $ "EASYVISION path: " ++ lib
    writeFile "hcontours.buildinfo" $
              "extra-lib-dirs: " ++ lib ++ arch ++"\n"

