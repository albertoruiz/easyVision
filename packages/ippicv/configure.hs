#! /usr/bin/env runhaskell

import System.Environment(getEnv)
import Image.Devel(getInclude)
import Distribution.System(buildArch,Arch(..))

arch = if buildArch == X86_64 then "intel64" else "ia32"

main = do
    evroot <- getEnv "EASYVISION"
    putStrLn $ "EASYVISION path: " ++ evroot

    incdir <- getInclude
    writeFile "hVision-ippicv.buildinfo" $ unlines
        [ incdir
        , "include-dirs: "   ++evroot++"/lib/ippicv_lnx/include"
        , "extra-lib-dirs: " ++evroot++"/lib/ippicv_lnx/lib/"++arch
        ]

