#! /usr/bin/env runhaskell

import System.Environment
import Util.Text
import Util.Misc(splitEvery)
import Util.Options
import Control.Applicative

main :: IO ()
main = do
    args <- cleanOpts <$> getArgs
    let rules = map f . splitEvery 2 $ args
          where
            f [a,b] = a :> b
            f _ = error "odd number of arguments in replace"
    rep <- getFlag "-r"
    if rep
      then do
        putStr =<< ioReplace (rules++[include,codefile]) =<< getContents
      else
        interact (replace rules)

