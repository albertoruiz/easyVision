#! /usr/bin/env runhaskell

> import Distribution.Simple
> import Distribution.Simple.PreProcess
> import Distribution.Simple.LocalBuildInfo
> import Distribution.PackageDescription
> import Distribution.Simple.Utils
> import System

> main =
>    defaultMainWithHooks
>           autoconfUserHooks{hookedPreProcessors=[("nms",transformation), ("dmy",donothing)]}

> transformation _ _ =
>   PreProcessor {
>     platformIndependent = True,
>     runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity ->
>       do info verbosity (inFile++" has been preprocessed to "++outFile)
>          system "cd lib/ImagProc/Ipp; runhaskell adapter"
>          return ()
>     }

> donothing _ _ =
>   PreProcessor {
>     platformIndependent = True,
>     runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity ->
>       do info verbosity (inFile++" has been pseudopreprocessed to "++outFile)
>          return ()
>     }

