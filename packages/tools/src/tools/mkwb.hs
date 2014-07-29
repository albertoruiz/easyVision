import System.Process
import System.Exit
import Util.Options
import System.Environment(getArgs)
import Control.Applicative((<$>))
import Control.Monad(when)
import Util.Text

main :: IO ()
main = do
    fname:_ <- cleanOpts <$> getArgs
    let rep = replace ["NAME":>fname]
    verbose <- getFlag "-v"
    keep <- getFlag "--keep"
    let info = when verbose . putStrLn

        step stepname command errormsg = do
            info stepname
            ok <- (system . rep) command
            when (ok/=ExitSuccess) $ (error . rep) errormsg        

    tex <- getFlag "--latex"
    if tex
      then latex step keep
      else html step keep


--------------------------------------------------------------------------------

html :: (String -> String -> String -> IO ()) -> Bool -> IO ()
html s k = do
    s "1. extract comments"
      "hextract NAME.hs NAME.clean.hs NAME.clean.aux.md"
      "problem in extraction step"

    s "2. include clean"
      "  hreplace --rules=\"!THIS=!HCODE\n!INCLUDE(NAME.clean.hs)\n!END\" < NAME.clean.aux.md > NAME.clean.md"
      "problem in autoinclude step"

    s "3. splice evaluation"
      "hsplice < NAME.clean.md > NAME.aux.hs" 
      "problem in splice"

    s "4. compile generator" 
      "ghc -O NAME.aux.hs -o NAME.aux.hsr 2> NAME.log > /dev/null"
      "errors in generator. See NAME.log"

    s "5. running generator"
      "./NAME.aux.hsr > NAME.aux.md"
      "problem running generator"

    s  "6. markdown/header"
     ( "  hreplace --rules='</pre>=\\n</pre>' < NAME.aux.md "
     ++"| markdown "
     ++"| hreplace --rules='<p>!TITLE X = #!REPLACE>{#!INCLUDE(HEADER)!WITH[title]=X}<p>;"
     ++"\\n</pre>=</pre>;MYSELF=NAME.hs'  > NAME.html" )
     "problem in markdown/header"

    when (not k) $
        s "7. delete temp files"
          "rm *.md *.o *.hi *.hsr *.aux.hs *.clean.hs NAME.log"
          "problem deleting temp files"


--------------------------------------------------------------------------------

latex :: (String -> String -> String -> IO ()) -> Bool -> IO ()
latex s k = do
    s "1. extract comments"
      "hextract NAME.hs NAME.clean.hs NAME.clean.aux.tex"
      "problem in extraction step"
    
    s "2. include clean"
     ("    hreplace --rules=\"!THIS=!HCODE\n!INCLUDE(NAME.clean.hs)\n!END\" < NAME.clean.aux.tex "
     ++ "| hreplace --latex > NAME.clean.tex")
     "problem in autoinclude step"

    s "3. splice evaluation"
      "hsplice < NAME.clean.tex > NAME.aux.hs" 
      "problem in splice"
    
    s "4. compile generator" 
      "ghc -O NAME.aux.hs -o NAME.aux.hsr 2> NAME.log > /dev/null"
      "errors in generator. See NAME.log"
    
    s "5. running generator"
      "./NAME.aux.hsr > NAME.aux.tex"
      "problem running generator"

    s "6. final replace"
      "hreplace --latex < NAME.aux.tex > NAME.tex"
      "problem in final replace"

    when (not k) $
        s "7. delete temp files"
          "rm *.aux.tex *.clean.tex *.o *.hi *.hsr *.aux.hs *.clean.hs NAME.log"
          "problem deleting temp files"

