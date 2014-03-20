import System.Environment
import Util.Text
import Util.Misc(splitEvery)
import Util.Options
import Control.Applicative
import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise

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
        putStr =<< ioReplace (rules++[include,codefile,hsfile]) =<< getContents
      else
        interact (replace rules)

hsfile :: Rule
hsfile = "HSFILE" :~> \f -> (return . highlight . addName f) =<< readFile f
  where
    highlight = hscolour HTML col False True "" False
    col = defaultColourPrefs
            { comment = [Italic, Dim, Foreground (Rgb 128 128 128)]
            , keyword = [Bold]
            , varop = [Foreground Green]
            , layout = [Normal]
            }
    addName f xs = replicate 0 ' ' ++ "-- "++ drop 3 f++"\n\n"++ xs

