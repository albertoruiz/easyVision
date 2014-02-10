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
        putStr =<< ioReplace (rules++[include,codefile']) =<< getContents
      else
        interact (replace rules)

codefile' :: Rule
codefile' = "CODEFILE" :~> \f -> (return . hscolour HTML col False True "" False) =<< readFile f
  where
    col = defaultColourPrefs
            { comment = [Italic, Dim, Foreground (Rgb 128 128 128)]
            , keyword = [Bold]
            , varop = [Foreground Green]
            , layout = [Normal]
            }
          

