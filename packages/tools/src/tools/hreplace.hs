import Util.Replace
import Util.Time(formattedDate)
import Util.Options

main :: IO ()
main = do
    puro <- getFlag "--pure"
    tex <- getFlag "--latex"
    grules <- optionString "--rules" ""
    let rules = parseRules grules ++ (if puro then [] else other tex)
    putStr . snd =<< ioReplace [] rules =<< getContents

other :: Bool -> [Rule]
other m = [date,include,ignore,define,quote,hscolour m,local]

date :: Rule
date = "!DATE" :> do t <- formattedDate; return ([],t)

